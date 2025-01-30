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
package net.codecadenza.runtime.richclient.javafx.search;

import static javafx.scene.layout.Region.USE_COMPUTED_SIZE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_COUNT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_RESET;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_CBO_EXACT_FILTER_MATCH;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_CHK_CASE_SENSITIVE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_CHK_COUNT_RECORDS;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_LBL_FIELD_NAME;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_LBL_FILTER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_LBL_OPERATOR;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_LBL_SORT_ORDER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_LIST_COLUMNS_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MAX_FETCH_SIZE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_COUNT_RES;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_COUNT_RES_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_ERR_CRITERION_EXP;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_BETWEEN;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_IN;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_ERR_NO_NUMBER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_MSG_ERR_NO_UUID;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_TAB_ADV_SETTINGS;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_TAB_FILTER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.SEARCH_INPUT_DIALOG_TITLE_MSG;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_SEARCH_TITLE;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_LIKE;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_IN;

import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javafx.collections.FXCollections;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.VPos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Control;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;
import javafx.scene.layout.VBox;
import javafx.stage.Window;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.control.AbstractProposalTextField;
import net.codecadenza.runtime.richclient.javafx.control.AbstractSelectionListPanel;
import net.codecadenza.runtime.richclient.javafx.control.LocalDateConverter;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogButtonType;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;
import net.codecadenza.runtime.richclient.javafx.dialog.TitleAreaDialog;
import net.codecadenza.runtime.richclient.transport.ServiceLocator;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchFieldTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import net.codecadenza.runtime.search.util.SearchOperatorHelper;
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
public class SearchInputDialog extends TitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final Pattern TOKEN_DELIMITER_IN_PATTERN = Pattern.compile(TOKEN_DELIMITER_IN);
	private static final Pattern TOKEN_DELIMITER_BETWEEN_PATTERN = Pattern.compile(TOKEN_DELIMITER_BETWEEN);

	private GridPane panFilter;
	private CheckBox chkExactFilterMatch;
	private CheckBox chkCountRecords;
	private ComboBox<Integer> cboFetchSize;
	private CheckBox chkCaseSensitive;
	private AbstractSelectionListPanel<SearchFieldDTO> listColumns;
	private final SearchDTO searchObj;
	private final HashMap<String, ComboBox<String>> operatorMap = new HashMap<>();
	private final HashMap<String, ComboBox<SortDirectionEnum>> sortMap = new HashMap<>();
	private final HashMap<String, Control> filterMap1 = new HashMap<>();
	private final HashMap<String, Control> filterMap2 = new HashMap<>();
	private final FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	private final Countable countable;
	private final DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	private final SimpleDateFormat dateTimeFormat = new SimpleDateFormat(userFormat.getDateTimeFormat());
	private final SimpleDateFormat dateFormat = new SimpleDateFormat(userFormat.getDateFormat());

	/**
	 * Constructor
	 * @param owner
	 * @param searchObj
	 * @param countable
	 */
	public SearchInputDialog(Window owner, SearchDTO searchObj, Countable countable) {
		super(owner, getTranslation(SEARCH_INPUT_DIALOG_TITLE));

		this.searchObj = searchObj;
		this.countable = countable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		setTitleImage(getImage(IMG_SEARCH_TITLE));
		setTitleMessage(getTranslation(SEARCH_INPUT_DIALOG_TITLE_MSG));

		final var tabPane = new TabPane();
		final var tabFilter = new Tab(getTranslation(SEARCH_INPUT_DIALOG_TAB_FILTER));

		tabPane.getTabs().add(tabFilter);

		panFilter = new GridPane();
		panFilter.setPadding(new Insets(5, 5, 5, 5));
		panFilter.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, 130.0, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panFilter.getColumnConstraints().add(
				new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panFilter.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, 80.0, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panFilter.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panFilter.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panFilter.setHgap(5);
		panFilter.setVgap(5);

		final var lblFieldName = new Label(getTranslation(SEARCH_INPUT_DIALOG_LBL_FIELD_NAME));
		lblFieldName.getStyleClass().add("field_column_title");

		panFilter.add(lblFieldName, 0, 0);

		final var lblOperator = new Label(getTranslation(SEARCH_INPUT_DIALOG_LBL_OPERATOR));
		lblOperator.getStyleClass().add("field_column_title");

		panFilter.add(lblOperator, 1, 0);

		final var lblSortOrder = new Label(getTranslation(SEARCH_INPUT_DIALOG_LBL_SORT_ORDER));
		lblSortOrder.getStyleClass().add("field_column_title");

		panFilter.add(lblSortOrder, 2, 0);

		final var lblFilter = new Label(getTranslation(SEARCH_INPUT_DIALOG_LBL_FILTER));
		lblFilter.getStyleClass().add("field_column_title");

		panFilter.add(lblFilter, 3, 0);

		tabFilter.setContent(panFilter);

		final var tabAdvSettings = new Tab(getTranslation(SEARCH_INPUT_DIALOG_TAB_ADV_SETTINGS));

		tabPane.getTabs().add(tabAdvSettings);

		final var panAdvSettings = new VBox(10);
		panAdvSettings.setPadding(new Insets(10, 0, 0, 0));

		tabAdvSettings.setContent(panAdvSettings);

		final var panAdvFields = new GridPane();
		panAdvFields.setHgap(10);
		panAdvFields.add(new Label(getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_MAX_FETCH_SIZE)), 0, 0);

		cboFetchSize = new ComboBox<>();
		cboFetchSize.getItems().add(10);
		cboFetchSize.getItems().add(100);
		cboFetchSize.getItems().add(1000);
		cboFetchSize.getItems().add(10000);
		cboFetchSize.getItems().add(100_000);
		cboFetchSize.setValue(searchObj.getMaxResult());

		chkExactFilterMatch = new CheckBox(getTranslation(SEARCH_INPUT_DIALOG_CBO_EXACT_FILTER_MATCH));
		chkExactFilterMatch.setSelected(searchObj.isExactFilterMatch());

		chkCountRecords = new CheckBox(getTranslation(SEARCH_INPUT_DIALOG_CHK_COUNT_RECORDS));
		chkCountRecords.setSelected(searchObj.isCount());

		chkCaseSensitive = new CheckBox(getTranslation(SEARCH_INPUT_DIALOG_CHK_CASE_SENSITIVE));
		chkCaseSensitive.setSelected(searchObj.isCaseSensitive());

		panAdvFields.add(cboFetchSize, 1, 0);
		panAdvFields.add(chkExactFilterMatch, 2, 0);
		panAdvFields.add(chkCountRecords, 3, 0);
		panAdvFields.add(chkCaseSensitive, 4, 0);

		listColumns = new AbstractSelectionListPanel<>(getTranslation(SEARCH_INPUT_DIALOG_LIST_COLUMNS_TITLE), false) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.control.AbstractSelectionListPanel#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(SearchFieldDTO element) {
				return element.getColLabel();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.control.AbstractSelectionListPanel#searchItems(java.lang.String)
			 */
			@Override
			public List<SearchFieldDTO> searchItems(String filter) throws Exception {
				return Collections.emptyList();
			}
		};

		panAdvSettings.getChildren().add(panAdvFields);
		panAdvSettings.getChildren().add(listColumns);

		VBox.setVgrow(listColumns, Priority.ALWAYS);

		initFilterFields();

		return tabPane;
	}

	/**
	 * @param searchField
	 * @return an array of strings
	 */
	private String[] splitFilterInput(SearchFieldDTO searchField) {
		String[] values;

		if (searchField.getOperator() == null) {
			values = new String[1];
			values[0] = searchField.getFilterCriteria();

			return values;
		}

		if (!searchField.getOperator().isExpectsArgument())
			return new String[0];

		if (searchField.getOperator().getValue().equals(OPERATOR_BETWEEN)) {
			if (TOKEN_DELIMITER_BETWEEN_PATTERN.split(searchField.getFilterCriteria()).length != 2)
				return new String[0];

			values = TOKEN_DELIMITER_BETWEEN_PATTERN.split(searchField.getFilterCriteria());
		}
		else {
			values = new String[1];
			values[0] = searchField.getFilterCriteria();
		}

		return values;
	}

	/**
	 * @param searchField
	 * @param rowIndex
	 */
	private void initBooleanField(SearchFieldDTO searchField, int rowIndex) {
		final var cboCrit1 = new ComboBox<String>();
		cboCrit1.getItems().add(Boolean.TRUE.toString().toLowerCase());
		cboCrit1.getItems().add(Boolean.FALSE.toString().toLowerCase());
		cboCrit1.getItems().add("");

		if (searchField.getFilterCriteria() != null)
			cboCrit1.setValue(searchField.getFilterCriteria());

		panFilter.add(cboCrit1, 3, rowIndex);
		filterMap1.put(searchField.getColLabel(), cboCrit1);
	}

	/**
	 * @param searchField
	 * @param rowIndex
	 */
	private void initEnumField(SearchFieldDTO searchField, int rowIndex) {
		final var cboCrit1 = new ComboBox<String>();
		cboCrit1.setItems(FXCollections.observableArrayList(searchField.getEnumListValues().values()));

		if (searchField.getFilterCriteria() != null)
			cboCrit1.setValue(searchField.getFilterCriteria());

		panFilter.add(cboCrit1, 3, rowIndex);
		filterMap1.put(searchField.getColLabel(), cboCrit1);
	}

	/**
	 * @param searchField
	 * @param rowIndex
	 */
	private void initDateField(SearchFieldDTO searchField, int rowIndex) {
		final var datePicker = new DatePicker();
		datePicker.setConverter(new LocalDateConverter());

		panFilter.add(datePicker, 3, rowIndex);
		filterMap1.put(searchField.getColLabel(), datePicker);

		final var datePicker2 = new DatePicker();
		datePicker2.setVisible(false);
		datePicker2.setConverter(new LocalDateConverter());

		panFilter.add(datePicker2, 4, rowIndex);
		filterMap2.put(searchField.getColLabel(), datePicker2);

		if (searchField.getOperator() != null && searchField.getOperator().getValue().equals(OPERATOR_BETWEEN))
			datePicker2.setVisible(true);

		int colIndex = 1;

		for (final String criterion : splitFilterInput(searchField)) {
			if (criterion == null || criterion.isEmpty())
				continue;

			var dateValue = new Date();

			try {
				if (searchField.isDateTimeFormat())
					dateValue = dateTimeFormat.parse(criterion);
				else
					dateValue = dateFormat.parse(criterion);
			}
			catch (final ParseException e) {
				logger.error("Could not parse criterion '{}'!", criterion, e);
			}

			var dp = (DatePicker) filterMap1.get(searchField.getColLabel());

			if (colIndex == 2)
				dp = (DatePicker) filterMap2.get(searchField.getColLabel());

			dp.setValue(dateValue.toInstant().atZone(ZoneId.systemDefault()).toLocalDate());

			colIndex++;
		}
	}

	/**
	 * @param searchField
	 * @param rowIndex
	 */
	private void initTextField(SearchFieldDTO searchField, int rowIndex) {
		if (searchField.getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL
				|| searchField.getDataType() == SearchFieldDataTypeEnum.DOUBLE
				|| searchField.getDataType() == SearchFieldDataTypeEnum.FLOAT
				|| searchField.getDataType() == SearchFieldDataTypeEnum.INTEGER
				|| searchField.getDataType() == SearchFieldDataTypeEnum.LONG) {
			final var txtCrit1 = new TextField();

			filterMap1.put(searchField.getColLabel(), txtCrit1);
			panFilter.add(txtCrit1, 3, rowIndex);

			final var txtCrit2 = new TextField();
			txtCrit2.setVisible(false);

			panFilter.add(txtCrit2, 4, rowIndex);
			filterMap2.put(searchField.getColLabel(), txtCrit2);

			if (searchField.getOperator() != null && searchField.getOperator().getValue().equals(OPERATOR_BETWEEN))
				txtCrit2.setVisible(true);

			int colIndex = 1;

			for (final String criterion : splitFilterInput(searchField)) {
				if (criterion == null || criterion.isEmpty())
					continue;

				var txtInput = (TextField) filterMap1.get(searchField.getColLabel());

				if (colIndex == 2)
					txtInput = (TextField) filterMap2.get(searchField.getColLabel());

				txtInput.setText(criterion);

				colIndex++;
			}
		}
		else {
			final var txtCrit1 = new TextField();
			txtCrit1.setText(searchField.getFilterCriteria());

			filterMap1.put(searchField.getColLabel(), txtCrit1);
			panFilter.add(txtCrit1, 3, rowIndex, 2, 1);
		}

		if (searchField.getLovCommand() != null && searchField.getDataType() == SearchFieldDataTypeEnum.STRING) {
			final var txtCrit1 = new AbstractProposalTextField<String>() {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.javafx.control.AbstractProposalTextField#getProposalText(java.lang.Object)
				 */
				@Override
				public String getProposalText(String item) {
					return item;
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.javafx.control.AbstractProposalTextField#getProposalItems(java.lang.String)
				 */
				@Override
				public List<String> getProposalItems(String textEntered) {
					final SearchService searchService = ServiceLocator.getService(SearchService.class);

					try {
						return searchService.getListOfValues(searchField.getLovCommand(), textEntered).stream().toList();
					}
					catch (final Exception e) {
						logger.error("Error while searching for proposal items by using input text '{}'!", textEntered, e);
					}

					return Collections.emptyList();
				}
			};

			filterMap1.put(searchField.getColLabel(), txtCrit1);
			panFilter.add(txtCrit1, 3, rowIndex, 2, 1);

			txtCrit1.setSelectedItem(searchField.getFilterCriteria());
		}
	}

	/**
	 * Add fields for operators and filter criteria dynamically
	 */
	private void initFilterFields() {
		int rowIndex = 1;
		final var visibleFields = new ArrayList<SearchFieldDTO>();
		final var hiddenFields = new ArrayList<SearchFieldDTO>();

		for (final SearchFieldDTO searchField : searchObj.getSearchFields()) {
			final String key = searchField.getColLabel();

			if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			if (searchField.isVisible())
				visibleFields.add(searchField);
			else
				hiddenFields.add(searchField);

			panFilter.getRowConstraints()
					.add(new RowConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, VPos.CENTER, false));

			final var lblField = new Label(searchField.getColLabel());

			panFilter.add(lblField, 0, rowIndex);

			final var cboOp = new ComboBox<String>();
			cboOp.setItems(SearchOperatorHelper.getOperatorsForField(searchField).stream().map(SearchOperatorDTO::getDescription)
					.collect(Collectors.toCollection(FXCollections::observableArrayList)));
			cboOp.getItems().add("");
			cboOp.setValue("");

			if (searchField.getOperator() != null)
				cboOp.setValue(searchField.getOperator().getDescription());

			panFilter.add(cboOp, 1, rowIndex);

			final var cboSort = new ComboBox<SortDirectionEnum>();
			cboSort.setItems(FXCollections.observableArrayList(SortDirectionEnum.values()));
			cboSort.setValue(searchField.getSortOrder());

			panFilter.add(cboSort, 2, rowIndex);
			operatorMap.put(key, cboOp);
			sortMap.put(key, cboSort);

			if (searchField.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
				initBooleanField(searchField, rowIndex);
			else if (searchField.getDataType() == SearchFieldDataTypeEnum.ENUM)
				initEnumField(searchField, rowIndex);
			else if (searchField.hasTemporalDataType())
				initDateField(searchField, rowIndex);
			else
				initTextField(searchField, rowIndex);

			// Disable the filter input control if the operator doesn't expect a filter criterion!
			if (searchField.getOperator() != null && !searchField.getOperator().isExpectsArgument()) {
				final Control c1 = filterMap1.get(searchField.getColLabel());
				c1.setDisable(true);

				final Control c2 = filterMap2.get(searchField.getColLabel());

				// We hide the second input control if it exists
				if (c2 != null)
					c2.setVisible(false);
			}

			// Add a listener for disabling filter input controls if the operator doesn't expect a filter criterion!
			cboOp.valueProperty().addListener((ov, oldValue, newValue) -> {
				filterMap1.get(key).setDisable(false);

				if (filterMap2.get(key) != null)
					filterMap2.get(key).setVisible(false);

				if (newValue.equals(SearchService.OPERATOR_IS_NULL) || newValue.equals(SearchService.OPERATOR_IS_NOT_NULL))
					filterMap1.get(key).setDisable(true);
				else if (newValue.equals(SearchService.OPERATOR_BETWEEN) && filterMap2.get(key) != null)
					filterMap2.get(key).setVisible(true);

				resetFieldInput(key);
			});

			rowIndex++;
		}

		listColumns.setSelectedItems(visibleFields);
		listColumns.setAvailableItems(hiddenFields);
	}

	/**
	 * Reset values of filter input fields
	 * @param key
	 */
	@SuppressWarnings("unchecked")
	private void resetFieldInput(String key) {
		final Control c = filterMap1.get(key);

		if (c instanceof final TextField textField)
			textField.setText("");
		else if (c instanceof final DatePicker datePicker)
			datePicker.setValue(null);
		else {
			final var comboBox = (ComboBox<String>) c;
			comboBox.setValue("");
		}

		final Control c2 = filterMap2.get(key);

		if (c2 == null)
			return;

		if (c2 instanceof final TextField textField)
			textField.setText("");
		else if (c2 instanceof final DatePicker datePicker)
			datePicker.setValue(null);
	}

	/**
	 * Reset filter criteria and operators
	 */
	private void reset() {
		final var selectedFields = new ArrayList<SearchFieldDTO>();

		for (final SearchFieldDTO searchField : searchObj.getSearchFields()) {
			if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			searchField.setFilterCriteria("");
			searchField.setOperator(null);
			searchField.setVisible(true);
			searchField.setSortOrder(SortDirectionEnum.NONE);
		}

		searchObj.setExactFilterMatch(true);
		searchObj.setCaseSensitive(false);
		searchObj.setCount(true);
		searchObj.setMaxResult(AbstractSearchGridView.DEFAULT_MAX_FETCH_SIZE);

		for (final SearchFieldDTO searchField : searchObj.getSearchFields()) {
			if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			operatorMap.get(searchField.getColLabel()).setValue("");
			sortMap.get(searchField.getColLabel()).setValue(searchField.getSortOrder());
			selectedFields.add(searchField);

			resetFieldInput(searchField.getColLabel());
		}

		listColumns.setAvailableItems(new ArrayList<>());
		listColumns.setSelectedItems(selectedFields);

		chkCaseSensitive.setSelected(searchObj.isCaseSensitive());
		chkCountRecords.setSelected(searchObj.isCount());
		chkExactFilterMatch.setSelected(searchObj.isExactFilterMatch());
		cboFetchSize.setValue(searchObj.getMaxResult());
	}

	/**
	 * Fill the search field using values of the corresponding input controls
	 * @param searchField
	 */
	@SuppressWarnings("unchecked")
	private void applyInput(SearchFieldDTO searchField) {
		if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
			return;

		final String key = searchField.getColLabel();
		final String op = operatorMap.get(key).getValue();

		searchField.setVisible(listColumns.getSelectedItems().contains(searchField));
		searchField.setSortOrder(sortMap.get(key).getValue());

		if (op.isEmpty())
			searchField.setOperator(null);
		else
			for (final SearchOperatorDTO sod : SearchOperatorHelper.getAllOperators())
				if (op.equals(sod.getDescription())) {
					searchField.setOperator(sod);
					break;
				}

		if (searchField.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
			final var cbo = (ComboBox<String>) filterMap1.get(key);

			if (cbo.getValue() == null || cbo.getValue().isEmpty())
				searchField.setFilterCriteria(null);
			else
				searchField.setFilterCriteria(cbo.getValue());
		}
		else if (searchField.getDataType() == SearchFieldDataTypeEnum.ENUM) {
			final var cbo = (ComboBox<String>) filterMap1.get(key);

			if (cbo.getValue() == null || cbo.getValue().isEmpty())
				searchField.setFilterCriteria(null);
			else
				for (final String enumLiteral : searchField.getEnumListValues().keySet())
					if (searchField.getEnumListValues().get(enumLiteral).equals(cbo.getValue()))
						searchField.setFilterCriteria(enumLiteral);
		}
		else if (searchField.hasTemporalDataType()) {
			final var datePicker1 = (DatePicker) filterMap1.get(key);
			final var datePicker2 = (DatePicker) filterMap2.get(key);

			if (datePicker1.getValue() != null) {
				final Date date1 = Date.from(Instant.from(datePicker1.getValue().atStartOfDay(ZoneId.systemDefault())));
				final Date date2;

				if (datePicker2.isVisible() && datePicker2.getValue() != null)
					date2 = Date.from(Instant.from(datePicker2.getValue().atStartOfDay(ZoneId.systemDefault())));
				else
					date2 = null;

				if (date2 != null) {
					if (searchField.isDateTimeFormat())
						searchField.setFilterCriteria(dateTimeFormat.format(date1) + TOKEN_DELIMITER_BETWEEN + dateTimeFormat.format(date2));
					else
						searchField.setFilterCriteria(dateFormat.format(date1) + TOKEN_DELIMITER_BETWEEN + dateFormat.format(date2));
				}
				else if (searchField.isDateTimeFormat())
					searchField.setFilterCriteria(dateTimeFormat.format(date1));
				else
					searchField.setFilterCriteria(dateFormat.format(date1));
			}
		}
		else {
			final var txtInput1 = (TextField) filterMap1.get(key);
			final var txtInput2 = (TextField) filterMap2.get(key);

			if (txtInput2 != null && txtInput2.isVisible())
				searchField.setFilterCriteria(txtInput1.getText() + TOKEN_DELIMITER_BETWEEN + txtInput2.getText());
			else
				searchField.setFilterCriteria(txtInput1.getText());
		}
	}

	/**
	 * @return true if the validation was successful
	 */
	private boolean validateInput() {
		// Save important field information temporarily in order to prevent saving illegal field data if the validation will fail!
		final var tempFieldList = new ArrayList<SearchFieldDTO>();

		for (final SearchFieldDTO searchField : searchObj.getSearchFields()) {
			if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			final var tempField = new SearchFieldDTO();
			tempField.setDataType(searchField.getDataType());
			tempField.setColLabel(searchField.getColLabel());
			tempField.setDateTimeFormat(searchField.isDateTimeFormat());
			tempField.setVisible(searchField.isVisible());
			tempField.setListOfValues(searchField.getListOfValues());
			tempField.setType(searchField.getType());
			tempField.setEnumListValues(searchField.getEnumListValues());

			applyInput(tempField);

			tempFieldList.add(tempField);
		}

		// Perform the input validation on the temporary fields
		for (final SearchFieldDTO searchField : tempFieldList) {
			if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			if (searchField.getOperator() != null) {
				// Check if the operator needs a filter criterion
				if (searchField.getOperator().isExpectsArgument()
						&& (searchField.getFilterCriteria() == null || searchField.getFilterCriteria().isEmpty())) {
					setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_CRITERION_EXP, searchField.getColLabel().toLowerCase()));
					return false;
				}

				// Validate the input if the 'in' operator is selected
				if (searchField.getOperator().getValue().equals(OPERATOR_IN)
						|| searchField.getOperator().getValue().equals(OPERATOR_NOT_IN)) {
					final var tokenizer = new StringTokenizer(searchField.getFilterCriteria(), TOKEN_DELIMITER_IN);

					if (tokenizer.countTokens() < 2) {
						setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_IN));
						return false;
					}
				}

				// Validate the input if the 'between' operator is selected
				if (searchField.getOperator().getValue().equals(OPERATOR_BETWEEN)
						&& TOKEN_DELIMITER_BETWEEN_PATTERN.split(searchField.getFilterCriteria()).length != 2) {
					setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_BETWEEN));
					return false;
				}
			}

			// Validate the input of either numeric or UUID fields
			if ((searchField.getDataType() == SearchFieldDataTypeEnum.LONG
					|| searchField.getDataType() == SearchFieldDataTypeEnum.INTEGER
					|| searchField.getDataType() == SearchFieldDataTypeEnum.FLOAT
					|| searchField.getDataType() == SearchFieldDataTypeEnum.DOUBLE
					|| searchField.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
					|| searchField.getDataType() == SearchFieldDataTypeEnum.UUID_STRING
					|| searchField.getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL) && searchField.getFilterCriteria() != null
					&& !searchField.getFilterCriteria().isEmpty()) {
				String[] values = splitFilterInput(searchField);

				if (searchField.getOperator() != null && (searchField.getOperator().getValue().equals(OPERATOR_NOT_IN)
						|| searchField.getOperator().getValue().equals(OPERATOR_IN)))
					values = TOKEN_DELIMITER_IN_PATTERN.split(searchField.getFilterCriteria());

				for (final String value : values) {
					try {
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
						else
							decimalFormat.parse(value);
					}
					catch (final NumberFormatException | ParseException e) {
						setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_NO_NUMBER, searchField.getColLabel().toLowerCase()));
						return false;
					}
					catch (final IllegalArgumentException e) {
						setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_NO_UUID, searchField.getColLabel().toLowerCase()));
						return false;
					}
				}
			}
		}

		return true;
	}

	/**
	 * Apply the input
	 */
	private void applyInput() {
		// Set filter criteria and operators
		searchObj.getSearchFields().forEach(this::applyInput);

		searchObj.setMaxResult(cboFetchSize.getValue());
		searchObj.setCaseSensitive(chkCaseSensitive.isSelected());
		searchObj.setCount(chkCountRecords.isSelected());
		searchObj.setExactFilterMatch(chkExactFilterMatch.isSelected());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOKPressed()
	 */
	@Override
	protected void onOKPressed() {
		returnCode = DialogButtonType.CANCEL;

		if (!validateInput())
			return;

		returnCode = DialogButtonType.OK;

		applyInput();

		close();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createButtonBar()
	 */
	@Override
	protected Node createButtonBar() {
		final Node parent = super.createButtonBar();
		final String title = getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_RES_TITLE);

		final Button cmdCount = addButton(DialogButtonType.COUNT, getTranslation(CMD_COUNT));
		cmdCount.setOnAction(e -> {
			try {
				if (!validateInput())
					return;

				applyInput();

				final long result = countable.countData();
				final String message = getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_RES, result);

				DialogUtil.openInformationDialog(this, title, message);
			}
			catch (final Exception ex) {
				logger.error("Error while performing count operation!", ex);

				final String message = getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR);

				DialogUtil.openErrorDialog(this, title, message, ex);
			}
		});

		final Button cmdReset = addButton(DialogButtonType.RESET, getTranslation(CMD_RESET));
		cmdReset.setOnAction(e -> reset());

		return parent;
	}

}
