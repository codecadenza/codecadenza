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
package net.codecadenza.runtime.webclient.vaadin.dialog;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ABSTRACT_LOV_DIALOG_LBL_INPUT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_RESET;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_SET;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_SEARCH_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UNEXPECTED_ERROR_TITLE;

import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.function.ValueProvider;
import jakarta.annotation.PostConstruct;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGridPanel;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;
import net.codecadenza.runtime.webclient.vaadin.provider.value.DateValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.DoubleValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.EnumValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.FloatValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.GregorianCalendarValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.LocalDateTimeValueProvider;
import net.codecadenza.runtime.webclient.vaadin.provider.value.LocalDateValueProvider;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract list-of-values dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the LOV dialog
 * @param <S> the type of the selected ID
 */
public abstract class AbstractLOVDialog<T, S> extends Dialog {
	private static final long serialVersionUID = -928554279366558222L;
	private static final String EMPTY_TITLE = "";

	protected final PreferencesStore preferences;
	protected final I18NService i18n;
	protected final TextField txtInput = new TextField();
	protected boolean applyValue;
	protected transient List<T> data = new ArrayList<>();
	protected transient S selectedId;
	protected String selectedDisplayValue;
	protected AbstractDataGridPanel<T> gridPanel;
	protected InternalI18NService internalI18n;

	/**
	 * Constructor
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractLOVDialog(I18NService i18n, PreferencesStore preferences) {
		this.i18n = i18n;
		this.preferences = preferences;

		setResizable(true);
		setWidth(700, Unit.PIXELS);
		setHeight(600, Unit.PIXELS);
	}

	/**
	 * Perform a search operation
	 * @param filter
	 * @return a list containing all items found
	 */
	protected abstract List<T> performSearchOperation(String filter);

	/**
	 * @param selectedItem
	 */
	protected abstract void setValues(T selectedItem);

	/**
	 * Initialize the grid columns
	 */
	protected abstract void initGridColumns();

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getDialogLogger();

	/**
	 * Initialize the dialog
	 */
	@PostConstruct
	protected void initDialog() {
		getDialogLogger().debug("Initialize dialog");

		internalI18n = new InternalI18NService(i18n.getLocale());

		getElement().setAttribute("theme", "dialog-with-title-bar");

		final var closeIcon = new Icon(VaadinIcon.CLOSE);
		closeIcon.addClickListener(listener -> this.close());

		final var divTitleBar = new Div();
		divTitleBar.addClassNames("titlebar-for-dialog", "draggable");
		divTitleBar.add(new Span(getTitle()), closeIcon);

		txtInput.setId("txtInput");
		txtInput.setWidth(200, Unit.PIXELS);
		txtInput.setValueChangeMode(ValueChangeMode.LAZY);
		txtInput.addValueChangeListener(event -> onFilterTextChanged(event.getValue()));

		addOpenedChangeListener(event -> txtInput.focus());

		final var flInput = new FormLayout();
		flInput.addFormItem(txtInput, internalI18n.getTranslationForFieldLabel(ABSTRACT_LOV_DIALOG_LBL_INPUT));

		gridPanel = new AbstractDataGridPanel<>(i18n, preferences) {
			private static final long serialVersionUID = -48965650320517620L;

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#initGridColumns()
			 */
			@Override
			public void initGridColumns() {
				AbstractLOVDialog.this.initGridColumns();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractGridPanel#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(T item) {
				if (item == null)
					return;

				setValues(item);

				applyValue = true;
				close();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#getDataGridLogger()
			 */
			@Override
			protected Logger getDataGridLogger() {
				return AbstractLOVDialog.this.getDialogLogger();
			}
		};

		gridPanel.setAddButtonBar(false);
		gridPanel.initComponent();
		gridPanel.setSizeFull();
		gridPanel.setPadding(false);
		gridPanel.initGridColumns();
		gridPanel.setId("dataTable");

		final var cmdSet = new Button(internalI18n.getTranslation(CMD_SET));

		cmdSet.addClickListener(event -> {
			if (gridPanel.getSelectedItem() == null)
				return;

			setValues(gridPanel.getSelectedItem());

			applyValue = true;
			close();
		});

		final var cmdReset = new Button(internalI18n.getTranslation(CMD_RESET));
		cmdReset.setId("cmdReset");

		cmdReset.addClickListener(event -> {
			selectedId = null;
			selectedDisplayValue = null;
			applyValue = true;

			close();
		});

		final var hlButtons = new HorizontalLayout();
		hlButtons.setHeight(100, Unit.PIXELS);
		hlButtons.add(cmdSet, cmdReset);

		final var vlContent = new VerticalLayout();
		vlContent.add(flInput, gridPanel, hlButtons);
		vlContent.setMargin(false);
		vlContent.setSizeFull();

		add(divTitleBar, vlContent);

		getDialogLogger().debug("Dialog initialization finished");
	}

	/**
	 * @return true if the selection made by the user should be applied to the corresponding input control of the caller
	 */
	public boolean isApplyValue() {
		return applyValue;
	}

	/**
	 * @return the selected ID
	 */
	public S getSelectedId() {
		return selectedId;
	}

	/**
	 * @return the selected display value
	 */
	public String getSelectedDisplayValue() {
		return selectedDisplayValue;
	}

	/**
	 * Add a column to the grid
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected <V> Column<T> addColumn(String columnKey, ValueProvider<T, V> valueProvider) {
		return gridPanel.addColumn(columnKey, valueProvider);
	}

	/**
	 * Add a column that is mapped to a field of type {@link Date}. The field values will be converted by using the user's date
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addDateColumn(String columnKey, ValueProvider<T, Date> valueProvider) {
		return addColumn(columnKey, new DateValueProvider<>(preferences.getDateFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Date}. The field values will be converted by using the user's date time
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addDateTimeColumn(String columnKey, ValueProvider<T, Date> valueProvider) {
		return addColumn(columnKey, new DateValueProvider<>(preferences.getDateTimeFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link LocalDate}. The field values will be converted by using the user's date
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addLocalDateColumn(String columnKey, ValueProvider<T, LocalDate> valueProvider) {
		return addColumn(columnKey, new LocalDateValueProvider<>(preferences.getDateFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link LocalDateTime}. The field values will be converted by using the user's
	 * date time format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addLocalDateTimeColumn(String columnKey, ValueProvider<T, LocalDateTime> valueProvider) {
		return addColumn(columnKey, new LocalDateTimeValueProvider<>(preferences.getDateTimeFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link GregorianCalendar}. The field values will be converted by using the
	 * user's date time format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addGregorianCalendarColumn(String columnKey, ValueProvider<T, GregorianCalendar> valueProvider) {
		return addColumn(columnKey, new GregorianCalendarValueProvider<>(preferences.getDateTimeFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Float}. The field values will be converted by using the user's number
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addFloatColumn(String columnKey, ValueProvider<T, Float> valueProvider) {
		return addColumn(columnKey, new FloatValueProvider<>(preferences.getNumberFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Double}. The field values will be converted by using the user's number
	 * format.
	 * @param columnKey
	 * @param valueProvider
	 * @return the new column
	 */
	protected Column<T> addDoubleColumn(String columnKey, ValueProvider<T, Double> valueProvider) {
		return addColumn(columnKey, new DoubleValueProvider<>(preferences.getNumberFormat(), valueProvider));
	}

	/**
	 * Add a column that is mapped to a field of type {@link Enum}. The field values will be converted by using the provided
	 * translation map.
	 * @param columnKey
	 * @param valueProvider
	 * @param translationMap
	 * @return the new column
	 */
	protected Column<T> addEnumColumn(String columnKey, ValueProvider<T, Enum<?>> valueProvider,
			Map<String, String> translationMap) {
		return addColumn(columnKey, new EnumValueProvider<>(translationMap, valueProvider));
	}

	/**
	 * @param filterText
	 */
	private void onFilterTextChanged(String filterText) {
		gridPanel.setData(new ArrayList<>());

		if (filterText.isEmpty()) {
			getDialogLogger().debug("Filter criterion is empty");

			return;
		}

		getDialogLogger().debug("Search for items by using filter '{}'", filterText);

		try {
			data = performSearchOperation(filterText);

			gridPanel.setData(data);
		}
		catch (final Exception e) {
			getDialogLogger().error("Searching for items failed!", e);

			final String title = internalI18n.getTranslation(MSG_UNEXPECTED_ERROR_TITLE);
			final String message = internalI18n.getTranslation(MSG_SEARCH_ERROR);

			new ErrorMessageDialog(title, message, e, internalI18n.getLocale()).open();
		}
	}

	/**
	 * @return the title of this dialog
	 */
	public String getTitle() {
		return EMPTY_TITLE;
	}

}
