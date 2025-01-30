/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.webclient.vaadin.search;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_SEARCH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_COUNT_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_COUNT_RESULT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_COUNT_TITLE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_SEARCH_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UNEXPECTED_ERROR_TITLE;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.webclient.vaadin.dialog.ErrorMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.dialog.InfoMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.util.Navigator;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;

/**
 * <p>
 * Component that provides a tabular view for given domain objects including an action for opening a dialog to enter filter and
 * sorting criteria
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractDataGridSearchView<T> extends AbstractDataGridView<T> {
	private static final long serialVersionUID = 6987466694243804218L;

	/**
	 * Constructor
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractDataGridSearchView(I18NService i18n, PreferencesStore preferences) {
		super(i18n, preferences);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#addButtonsToButtonBar(com.vaadin.ui.
	 * HorizontalLayout)
	 */
	@Override
	public void addButtonsToButtonBar(HorizontalLayout hlButtonBar) {
		super.addButtonsToButtonBar(hlButtonBar);

		final var cmdSearch = new Button(internalI18n.getTranslation(CMD_SEARCH));
		cmdSearch.setIcon(new Icon(VaadinIcon.SEARCH));
		cmdSearch.setId("cmdSearch");
		cmdSearch.addClickListener(event -> openSearchInputDialog());

		hlButtonBar.add(cmdSearch);
	}

	/**
	 * Open the search input dialog
	 */
	private void openSearchInputDialog() {
		final var dlg = new SearchInputDialog(searchObj, internalI18n.getLocale());

		dlg.addOpenedChangeListener(event -> {
			if (!event.isOpened())
				onSearchDialogClose(dlg);
		});

		dlg.open();
	}

	/**
	 * Get the string ID parameter from the provided route parameters
	 * @param event
	 * @return the ID or null if the parameter was not found
	 */
	public String getIdParameter(BeforeEnterEvent event) {
		final String id = event.getRouteParameters().get(Navigator.ROUTE_PARAMETER_ID).orElse(null);

		if (id == null)
			return null;

		return URLDecoder.decode(id, StandardCharsets.UTF_8);
	}

	/**
	 * @param dlg
	 */
	private void onSearchDialogClose(SearchInputDialog dlg) {
		if (dlg.getMode() == SearchInputDialog.OperationMode.NONE)
			return;

		if (dlg.getMode() == SearchInputDialog.OperationMode.COUNT) {
			getDataGridLogger().debug("Perform count operation");

			try {
				final long countResult = performCountOperation();

				final String message = internalI18n.getTranslation(MSG_COUNT_RESULT, countResult);
				final String title = internalI18n.getTranslation(MSG_COUNT_TITLE);

				new InfoMessageDialog(title, message, internalI18n.getLocale()).open();
			}
			catch (final Exception ex) {
				getDataGridLogger().error("Error while performing count operation!", ex);

				final String title = internalI18n.getTranslation(MSG_UNEXPECTED_ERROR_TITLE);
				final String message = internalI18n.getTranslation(MSG_COUNT_ERROR);

				new ErrorMessageDialog(title, message, ex, internalI18n.getLocale()).open();
			}
		}
		else {
			try {
				getDataGridLogger().debug("Perform search operation");

				final Long startTime = System.currentTimeMillis();

				data = fetchData();

				for (final SearchFieldDTO field : searchObj.getSearchFields())
					for (final Column<T> column : grid.getColumns()) {
						if (!column.getKey().equals(field.getColLabel()))
							continue;

						column.setVisible(field.isVisible());
					}

				grid.setItems(data);

				final Long endTime = System.currentTimeMillis();
				final Long timeDif = endTime - startTime;
				Long countResult = null;

				if (searchObj.isCount())
					countResult = performCountOperation();

				lblFooter.setText(buildFooterMessage((long) data.size(), countResult, timeDif));
			}
			catch (final Exception ex) {
				getDataGridLogger().error("Error while performing search operation!", ex);

				final String title = internalI18n.getTranslation(MSG_UNEXPECTED_ERROR_TITLE);
				final String message = internalI18n.getTranslation(MSG_SEARCH_ERROR);

				new ErrorMessageDialog(title, message, ex, internalI18n.getLocale()).open();
			}
		}
	}

}
