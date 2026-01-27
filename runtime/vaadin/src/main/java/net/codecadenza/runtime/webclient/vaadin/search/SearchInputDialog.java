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

import static net.codecadenza.runtime.search.SearchService.OPERATOR_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_LIKE;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_IN;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_CANCEL;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_COUNT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_OK;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_RESET;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_VALIDATION_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_CBO_FETCHSIZE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_CHK_CASESENSITIVE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_CHK_COUNT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_CHK_EXACTFILTER;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_TAB_PAGE_ADDSETTINGS;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_TAB_PAGE_FILTER;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.SEARCH_INPUT_DIALOG_TITLE;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ModalityMode;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.formlayout.FormLayout.ResponsiveStep;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.regex.Pattern;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import net.codecadenza.runtime.search.util.SearchOperatorHelper;
import net.codecadenza.runtime.webclient.vaadin.component.TabSheet;
import net.codecadenza.runtime.webclient.vaadin.converter.LocalDateTimeToStringConverter;
import net.codecadenza.runtime.webclient.vaadin.converter.LocalDateToStringConverter;
import net.codecadenza.runtime.webclient.vaadin.converter.StringToTranslationMapConverter;
import net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog;
import net.codecadenza.runtime.webclient.vaadin.dialog.InfoMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Search input dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchInputDialog extends AbstractTitleDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final Pattern TOKEN_DELIMITER_IN_PATTERN = Pattern.compile(TOKEN_DELIMITER_IN);
	private static final Pattern TOKEN_DELIMITER_BETWEEN_PATTERN = Pattern.compile(TOKEN_DELIMITER_BETWEEN);
	private static final long serialVersionUID = -2140658632003840713L;
	private static final int DEFAULT_FETCH_SIZE = 1000;

	public enum OperationMode {
		NONE, COUNT, SEARCH
	}

	private final SearchDTO searchObj;
	private final Binder<SearchDTO> binder = new Binder<>();
	private final HashMap<SearchFieldDTO, Binder<SearchFieldDTO>> fieldBinders = new HashMap<>();
	private OperationMode mode = OperationMode.NONE;

	/**
	 * Constructor
	 * @param searchObj
	 * @param locale
	 */
	public SearchInputDialog(SearchDTO searchObj, Locale locale) {
		super(new InternalI18NService(locale).getTranslation(SEARCH_INPUT_DIALOG_TITLE), locale);

		this.searchObj = searchObj;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.dialog.Dialog#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		super.onAttach(attachEvent);

		logger.debug("Initialize dialog");

		setWidth(750, Unit.PIXELS);
		setHeight(500, Unit.PIXELS);
		setResizable(true);
		setModality(ModalityMode.STRICT);

		final var flSearchInput = new FormLayout();
		flSearchInput.getStyle().set("display", "block");
		flSearchInput.setResponsiveSteps(new ResponsiveStep("100px", 1), new ResponsiveStep("500px", 4));

		final var scrollLayout = new VerticalLayout(flSearchInput);
		scrollLayout.getStyle().set("overflow", "auto");
		scrollLayout.setPadding(false);
		scrollLayout.setWidthFull();
		scrollLayout.setHeightFull();
		scrollLayout.setHeight("350px");

		int fieldIndex = 0;

		// Add a row for every search field
		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			logger.debug("Add field '{}'", field.getColLabel());

			final var fieldBinder = new Binder<SearchFieldDTO>();

			// Create a combobox for selecting the search operator
			final Component cboOperator = createSearchOperatorField(field, fieldIndex, fieldBinder);

			// Create a combobox for selecting the sort order
			final var cboSortOrder = new ComboBox<SortDirectionEnum>();
			cboSortOrder.setAllowCustomValue(false);
			cboSortOrder.setWidth(80, Unit.PIXELS);
			cboSortOrder.setItems(SortDirectionEnum.values());
			cboSortOrder.setId("s_" + fieldIndex);

			// Create a filter input field
			final Component filterField = createFilterInputField(field, fieldIndex, fieldBinder);

			// Create a checkbox for selecting if a column should be either visible or hidden
			final var chkVisible = new Checkbox();
			chkVisible.setLabel(field.getColLabel());

			fieldBinder.forField(cboSortOrder).bind(SearchFieldDTO::getSortOrder, SearchFieldDTO::setSortOrder);
			fieldBinder.forField(chkVisible).bind(SearchFieldDTO::isVisible, SearchFieldDTO::setVisible);

			fieldIndex++;

			fieldBinders.put(field, fieldBinder);

			flSearchInput.add(chkVisible, cboOperator, cboSortOrder, filterField);
		}

		final var chkCount = new Checkbox();
		final var chkCasesensitive = new Checkbox();
		final var chkExactFilter = new Checkbox();

		final var cboFetchSize = new ComboBox<Integer>();
		cboFetchSize.setWidth(100, Unit.PIXELS);
		cboFetchSize.setItems(Arrays.asList(10, 100, 1000, 10000));

		binder.forField(chkCount).bind(SearchDTO::isCount, SearchDTO::setCount);
		binder.forField(chkCasesensitive).bind(SearchDTO::isCaseSensitive, SearchDTO::setCaseSensitive);
		binder.forField(chkExactFilter).bind(SearchDTO::isExactFilterMatch, SearchDTO::setExactFilterMatch);
		binder.forField(cboFetchSize).bind(SearchDTO::getMaxResult, SearchDTO::setMaxResult);

		final var flAddSettings = new FormLayout();
		flAddSettings.addFormItem(chkCount, i18n.getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_CHK_COUNT));
		flAddSettings.addFormItem(chkCasesensitive, i18n.getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_CHK_CASESENSITIVE));
		flAddSettings.addFormItem(chkExactFilter, i18n.getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_CHK_EXACTFILTER));
		flAddSettings.addFormItem(cboFetchSize, i18n.getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_CBO_FETCHSIZE));

		final var tabSheet = new TabSheet(this);
		tabSheet.addTab(scrollLayout, i18n.getTranslation(SEARCH_INPUT_DIALOG_TAB_PAGE_FILTER));
		tabSheet.addTab(flAddSettings, i18n.getTranslation(SEARCH_INPUT_DIALOG_TAB_PAGE_ADDSETTINGS));

		binder.readBean(searchObj);
		fieldBinders.entrySet().forEach(entry -> entry.getValue().readBean(entry.getKey()));

		logger.debug("Dialog initialization finished");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog#
	 * addButtons(com.vaadin.flow.component.dialog.Dialog.DialogFooter)
	 */
	@Override
	protected void addButtons(DialogFooter dialogFooter) {
		final var cmdOK = new Button(i18n.getTranslation(CMD_OK));
		cmdOK.setId("cmdOK");
		cmdOK.addClickListener(_ -> applyFormData(OperationMode.SEARCH));

		final var cmdCount = new Button(i18n.getTranslation(CMD_COUNT));
		cmdCount.setId("cmdCount");
		cmdCount.addClickListener(_ -> applyFormData(OperationMode.COUNT));

		final var cmdReset = new Button(i18n.getTranslation(CMD_RESET));
		cmdReset.setId("cmdReset");
		cmdReset.addClickListener(_ -> resetFields());

		final var cmdCancel = new Button(i18n.getTranslation(CMD_CANCEL));
		cmdCancel.setId("cmdCancel");
		cmdCancel.addClickListener(_ -> close());

		dialogFooter.add(cmdOK, cmdCount, cmdReset, cmdCancel);
	}

	/**
	 * @return the selected operation mode
	 */
	public OperationMode getMode() {
		return mode;
	}

	/**
	 * Create a combobox that is filled with appropriate search operators based on the provided search field
	 * @param field
	 * @param fieldIndex
	 * @param fieldBinder
	 * @return a reference to the created component
	 */
	private Component createSearchOperatorField(SearchFieldDTO field, int fieldIndex, Binder<SearchFieldDTO> fieldBinder) {
		final var cboOperator = new ComboBox<SearchOperatorDTO>();
		cboOperator.setAllowCustomValue(false);
		cboOperator.setWidth(130, Unit.PIXELS);
		cboOperator.setId("o_" + fieldIndex);
		cboOperator.setItemLabelGenerator(SearchOperatorDTO::getDescription);

		final var operatorList = new ArrayList<SearchOperatorDTO>();

		for (final SearchOperatorDTO op : SearchOperatorHelper.getOperatorsForField(field)) {
			final String operator = op.getDescription();
			final var numericType = field.getDataType() == SearchFieldDataTypeEnum.LONG
					|| field.getDataType() == SearchFieldDataTypeEnum.INTEGER || field.getDataType() == SearchFieldDataTypeEnum.FLOAT
					|| field.getDataType() == SearchFieldDataTypeEnum.DOUBLE || field.getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL;
			final var uuidType = field.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
					|| field.getDataType() == SearchFieldDataTypeEnum.UUID_STRING;

			// The 'BETWEEN' operator is only supported for numeric fields
			if (operator.equals(SearchService.OPERATOR_BETWEEN) && !numericType)
				continue;

			// The 'IN' and 'NOT_IN' operators are only supported for numeric and UUID fields
			if ((operator.equals(SearchService.OPERATOR_IN) || operator.equals(SearchService.OPERATOR_NOT_IN)) && !numericType
					&& !uuidType)
				continue;

			operatorList.add(op);
		}

		cboOperator.setItems(operatorList);

		fieldBinder.forField(cboOperator).bind(SearchFieldDTO::getOperator, SearchFieldDTO::setOperator);

		return cboOperator;
	}

	/**
	 * Create a filter input field based on the given search field
	 * @param field
	 * @param fieldIndex
	 * @param fieldBinder
	 * @return a reference to the created component
	 */
	private Component createFilterInputField(SearchFieldDTO field, int fieldIndex, Binder<SearchFieldDTO> fieldBinder) {
		if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
			final var cboFilterBoolean = new ComboBox<String>();
			cboFilterBoolean.setAllowCustomValue(false);
			cboFilterBoolean.setWidth(80, Unit.PIXELS);
			cboFilterBoolean.setItems(Arrays.asList(Boolean.FALSE.toString(), Boolean.TRUE.toString()));
			cboFilterBoolean.setId("fc_" + fieldIndex);

			fieldBinder.forField(cboFilterBoolean).bind(SearchFieldDTO::getFilterCriteria, SearchFieldDTO::setFilterCriteria);

			return cboFilterBoolean;
		}
		else if (field.getDataType() == SearchFieldDataTypeEnum.DATE
				|| field.getDataType() == SearchFieldDataTypeEnum.GREGORIAN_CALENDAR) {
			if (field.isDateTimeFormat())
				return addDateTimeField(fieldIndex, fieldBinder);

			return addDateField(fieldIndex, fieldBinder);
		}
		else if (field.getDataType() == SearchFieldDataTypeEnum.LOCAL_DATE)
			return addDateField(fieldIndex, fieldBinder);
		else if (field.getDataType() == SearchFieldDataTypeEnum.LOCAL_DATE_TIME)
			return addDateTimeField(fieldIndex, fieldBinder);
		else if (field.getDataType() == SearchFieldDataTypeEnum.ENUM) {
			final var cboEnum = new ComboBox<String>();
			cboEnum.setAllowCustomValue(false);
			cboEnum.setWidth(120, Unit.PIXELS);
			cboEnum.setId("fc_" + fieldIndex);
			cboEnum.setItems(field.getEnumListValues().values());

			fieldBinder.forField(cboEnum)
					.withConverter(new StringToTranslationMapConverter<>(field.getEnumListValues(), i18n.getLocale(), true))
					.bind(SearchFieldDTO::getFilterCriteria, SearchFieldDTO::setFilterCriteria);

			return cboEnum;
		}
		else {
			final var txtInput = new TextField();
			txtInput.setWidth(300, Unit.PIXELS);
			txtInput.setId("fi_" + fieldIndex);

			fieldBinder.forField(txtInput).bind(SearchFieldDTO::getFilterCriteria, SearchFieldDTO::setFilterCriteria);

			return txtInput;
		}
	}

	/**
	 * Add a date input field to the search input dialog
	 * @param fieldIndex
	 * @param fieldBinder
	 * @return the component
	 */
	private Component addDateField(int fieldIndex, Binder<SearchFieldDTO> fieldBinder) {
		final var txtDate = new DatePicker();
		txtDate.setWidth(300, Unit.PIXELS);
		txtDate.setId("fd_" + fieldIndex);

		fieldBinder.forField(txtDate).withConverter(new LocalDateToStringConverter(searchObj.getDateFormat()))
				.bind(SearchFieldDTO::getFilterCriteria, SearchFieldDTO::setFilterCriteria);

		return txtDate;
	}

	/**
	 * Add a date time input field to the search input dialog
	 * @param fieldIndex
	 * @param fieldBinder
	 * @return the component
	 */
	private Component addDateTimeField(int fieldIndex, Binder<SearchFieldDTO> fieldBinder) {
		final var txtDateTime = new DateTimePicker();
		txtDateTime.setWidth(300, Unit.PIXELS);
		txtDateTime.setId("fd_" + fieldIndex);

		fieldBinder.forField(txtDateTime).withConverter(new LocalDateTimeToStringConverter(searchObj.getDateTimeFormat()))
				.bind(SearchFieldDTO::getFilterCriteria, SearchFieldDTO::setFilterCriteria);

		return txtDateTime;
	}

	/**
	 * @param mode
	 */
	private void applyFormData(OperationMode mode) {
		try {
			binder.writeBean(searchObj);

			for (final Entry<SearchFieldDTO, Binder<SearchFieldDTO>> entry : fieldBinders.entrySet()) {
				entry.getValue().writeBean(entry.getKey());

				validateSearchInputField(entry.getKey());
			}

			this.mode = mode;

			close();
		}
		catch (final Exception e) {
			logger.debug("Input validation failed!", e);

			final String title = i18n.getTranslation(SEARCH_INPUT_DIALOG_TITLE);
			final String message = i18n.getTranslation(MSG_VALIDATION_ERROR);

			new InfoMessageDialog(title, message, i18n.getLocale()).open();
		}
	}

	/**
	 * Check the entered filter data of a search field
	 * @param searchField
	 * @throws ParseException if an input value could not be converted to a number
	 * @throws IllegalArgumentException if an input value could not be converted to a {@link UUID}
	 * @throws NumberFormatException if an input value could not be converted to an integer
	 */
	private void validateSearchInputField(final SearchFieldDTO searchField) throws ParseException {
		final String[] values = splitFilterInput(searchField);

		for (final String value : values) {
			if (value == null || value.isEmpty())
				continue;

			if (searchField.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
					|| searchField.getDataType() == SearchFieldDataTypeEnum.UUID_STRING) {
				boolean checkInput = false;

				if (searchField.getOperator() == null && searchField.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY)
					checkInput = true;

				if (searchField.getOperator() != null && !searchField.getOperator().getValue().equals(OPERATOR_LIKE)
						&& !searchField.getOperator().getValue().equals(OPERATOR_NOT_LIKE))
					checkInput = true;

				if (checkInput)
					UUID.fromString(value);
			}
			else if (searchField.getDataType() == SearchFieldDataTypeEnum.LONG
					|| searchField.getDataType() == SearchFieldDataTypeEnum.INTEGER)
				Long.parseLong(value);
			else if (searchField.getDataType() == SearchFieldDataTypeEnum.FLOAT
					|| searchField.getDataType() == SearchFieldDataTypeEnum.DOUBLE
					|| searchField.getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL)
				new DecimalFormat(searchObj.getNumberFormat()).parse(value);
		}
	}

	/**
	 * Extract the dedicated filter values of a search field
	 * @param searchField
	 * @return an array of strings
	 * @throws IllegalArgumentException if a filter value is either missing or not expected
	 */
	private String[] splitFilterInput(SearchFieldDTO searchField) {
		String[] values;

		if (searchField.getOperator() == null) {
			values = new String[1];
			values[0] = searchField.getFilterCriteria();

			return values;
		}

		if (!searchField.getOperator().isExpectsArgument() && searchField.getFilterCriteria() != null
				&& !searchField.getFilterCriteria().isEmpty())
			throw new IllegalArgumentException("Operator doesn't expect filter input!");

		if (searchField.getOperator().isExpectsArgument()
				&& (searchField.getFilterCriteria() == null || searchField.getFilterCriteria().isEmpty()))
			throw new IllegalArgumentException("Operator requires filter input!");

		if (searchField.getOperator().getValue().equals(OPERATOR_BETWEEN)) {
			if (TOKEN_DELIMITER_BETWEEN_PATTERN.split(searchField.getFilterCriteria()).length != 2)
				throw new IllegalArgumentException("No valid input for between-operator!");

			values = TOKEN_DELIMITER_BETWEEN_PATTERN.split(searchField.getFilterCriteria());
		}
		else if (searchField.getOperator().getValue().equals(OPERATOR_NOT_IN)
				|| searchField.getOperator().getValue().equals(OPERATOR_IN))
			values = TOKEN_DELIMITER_IN_PATTERN.split(searchField.getFilterCriteria());
		else {
			values = new String[1];
			values[0] = searchField.getFilterCriteria();
		}

		return values;
	}

	/**
	 * Reset all fields of this dialog
	 */
	private void resetFields() {
		searchObj.setCaseSensitive(false);
		searchObj.setCount(true);
		searchObj.setExactFilterMatch(true);
		searchObj.setMaxResult(DEFAULT_FETCH_SIZE);

		searchObj.getSearchFields().forEach(searchField -> {
			searchField.setFilterCriteria(null);
			searchField.setOperator(null);
			searchField.setSortOrder(SortDirectionEnum.NONE);
			searchField.setVisible(true);
		});

		binder.readBean(searchObj);

		fieldBinders.entrySet().forEach(entry -> entry.getValue().readBean(entry.getKey()));
	}

}
