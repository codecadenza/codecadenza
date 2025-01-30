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
package net.codecadenza.runtime.webclient.vaadin.component;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.DUAL_FILTERING_LIST_FIELD_LBL_INPUT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_LOOKUP_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UNEXPECTED_ERROR_TITLE;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.AbstractBackEndDataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import java.lang.invoke.MethodHandles;
import java.util.Collections;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.dialog.ErrorMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.provider.data.BackEndFilteringDataProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Component for searching and selecting items in a list
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type that is used by this component
 */
public class DualFilteringListField<T> extends AbstractDualListField<T> {
	private static final long serialVersionUID = 4846805117051271506L;
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private BackEndFilteringDataProvider<T> dataProvider;

	/**
	 * Constructor
	 * @param locale
	 */
	public DualFilteringListField(Locale locale) {
		super(locale);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDualListField#getDataProvider()
	 */
	@Override
	public AbstractBackEndDataProvider<T, String> getDataProvider() {
		return dataProvider;
	}

	/**
	 * @param dataProvider
	 */
	public void setDataProvider(BackEndFilteringDataProvider<T> dataProvider) {
		this.dataProvider = dataProvider;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDualListField#onAttach(com.vaadin.flow.component.
	 * AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		final var txtFilter = new TextField();
		txtFilter.setValueChangeMode(ValueChangeMode.LAZY);
		txtFilter.setWidth(200, Unit.PIXELS);
		txtFilter.addValueChangeListener(event -> onFilterTextChanged(event.getValue()));

		final var flInput = new FormLayout();
		flInput.addFormItem(txtFilter, i18n.getTranslationForFieldLabel(DUAL_FILTERING_LIST_FIELD_LBL_INPUT));

		vlContent.add(flInput);

		// Call super() in order to add all other components!
		super.onAttach(attachEvent);
	}

	/**
	 * Refresh the source list by fetching the items that match the provided filter text
	 * @param filterText
	 */
	private void onFilterTextChanged(String filterText) {
		try {
			if (filterText.isEmpty()) {
				gridSource.setItems(Collections.emptyList());
				return;
			}

			logger.debug("Search for items by using filter '{}'", filterText);

			gridSource.setItems(dataProvider.getFetchCallback().fetchData(filterText));
		}
		catch (final Exception e) {
			logger.error("Searching for items failed!", e);

			final String title = i18n.getTranslation(MSG_UNEXPECTED_ERROR_TITLE);
			final String message = i18n.getTranslation(MSG_LOOKUP_ERROR);

			new ErrorMessageDialog(title, message, e, i18n.getLocale()).open();
		}
	}

}
