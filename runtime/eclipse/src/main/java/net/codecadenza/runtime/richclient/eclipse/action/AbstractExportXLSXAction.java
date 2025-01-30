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
package net.codecadenza.runtime.richclient.eclipse.action;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.EXPORT_TO_XLS_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.search.__AbstractGridResultPanel;
import net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView;
import net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.util.ColumnSorter;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import org.apache.poi.ss.SpreadsheetVersion;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Base class for actions that export the table content to a Microsoft Excel file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractExportXLSXAction extends Action {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String TRUE = "boolean_true";
	private static final String FALSE = "boolean_false";
	private static final String DEFAULT_SHEET_NAME = "Sheet 1";
	protected static final String DEFAULT_FILE_EXT = ".xlsx";

	protected Shell parentShell;
	private Table table;
	private SearchDTO searchObj;
	private final HashMap<Integer, String> colIdMap = new HashMap<>();

	private enum InitMode {
		VIA_COLUMN_SORTER, VIA_SEARCH_DTO, UNDEF
	}

	private InitMode mode = InitMode.UNDEF;
	private final HashSet<Integer> dateColumnIndexSet = new HashSet<>();

	/**
	 * Constructor
	 * @param parentShell
	 */
	protected AbstractExportXLSXAction(Shell parentShell) {
		this.setToolTipText(getTranslation(EXPORT_TO_XLS_ACTION_SHORT_DESC));
		this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_EXCEL));
		this.parentShell = parentShell;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param table
	 * @param searchObj
	 */
	protected AbstractExportXLSXAction(Shell parentShell, Table table, SearchDTO searchObj) {
		this(parentShell);

		this.table = table;
		this.searchObj = searchObj;
		this.mode = InitMode.VIA_SEARCH_DTO;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param grid
	 */
	protected AbstractExportXLSXAction(Shell parentShell, __AbstractDataGridComposite<?> grid) {
		this(parentShell);

		this.table = grid.getTableViewer().getTable();
		this.mode = InitMode.VIA_COLUMN_SORTER;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param panel
	 */
	protected AbstractExportXLSXAction(Shell parentShell, __AbstractGridResultPanel<?> panel) {
		this(parentShell);

		this.table = panel.getTableViewer().getTable();
		this.mode = InitMode.VIA_COLUMN_SORTER;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param view
	 */
	protected AbstractExportXLSXAction(Shell parentShell, __AbstractResultView<?> view) {
		this(parentShell);

		this.searchObj = view.getSearchObj();
		this.table = view.getTable();
		this.mode = InitMode.VIA_SEARCH_DTO;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param searchView
	 */
	protected AbstractExportXLSXAction(Shell parentShell, __AbstractSearchResultView<?> searchView) {
		this(parentShell);

		this.searchObj = searchView.getSearchObj();
		this.table = searchView.getTable();
		this.mode = InitMode.VIA_SEARCH_DTO;
	}

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

	/**
	 * @return a hash map of all column headers with their data types
	 */
	private HashMap<String, Class<?>> initTypes() {
		final var typeMap = new HashMap<String, Class<?>>();

		if (mode == InitMode.VIA_COLUMN_SORTER) {
			for (final TableColumn col : table.getColumns()) {
				for (final Listener l : col.getListeners(SWT.Selection)) {
					if (l instanceof final ColumnSorter sorter) {
						if (sorter.getSortType() == ColumnSortType.BOOLEAN)
							typeMap.put(col.getText(), Boolean.class);
						else if (sorter.getSortType() == ColumnSortType.DECIMAL)
							typeMap.put(col.getText(), Double.class);
						else if (sorter.getSortType() == ColumnSortType.DATE)
							typeMap.put(col.getText(), Date.class);
						else if (sorter.getSortType() == ColumnSortType.DATETIME)
							typeMap.put(col.getText(), Calendar.class);
						else if (sorter.getSortType() == ColumnSortType.INTEGER)
							typeMap.put(col.getText(), Long.class);
						else if (sorter.getSortType() == ColumnSortType.STRING)
							typeMap.put(col.getText(), String.class);
					}
				}
			}
		}
		else if (mode == InitMode.VIA_SEARCH_DTO) {
			for (final SearchFieldDTO field : searchObj.getSearchFields()) {
				// We are not interested in invisible columns
				if (!field.isVisible())
					continue;

				if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
					typeMap.put(field.getColLabel(), Boolean.class);
				else if (field.getDataType() == SearchFieldDataTypeEnum.CHAR)
					typeMap.put(field.getColLabel(), String.class);
				else if (field.getDataType() == SearchFieldDataTypeEnum.DOUBLE
						|| field.getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL || field.getDataType() == SearchFieldDataTypeEnum.FLOAT)
					typeMap.put(field.getColLabel(), Double.class);
				else if (field.getDataType() == SearchFieldDataTypeEnum.ENUM)
					typeMap.put(field.getColLabel(), String.class);
				else if (field.getDataType() == SearchFieldDataTypeEnum.INTEGER || field.getDataType() == SearchFieldDataTypeEnum.LONG)
					typeMap.put(field.getColLabel(), Long.class);
				else if (field.getDataType() == SearchFieldDataTypeEnum.STRING
						|| field.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
						|| field.getDataType() == SearchFieldDataTypeEnum.UUID_STRING)
					typeMap.put(field.getColLabel(), String.class);
				else if (field.hasTemporalDataType()) {
					// Per definition, we use Calendar for date time cells and Date for cells containing date values!
					if (field.isDateTimeFormat())
						typeMap.put(field.getColLabel(), Calendar.class);
					else
						typeMap.put(field.getColLabel(), Date.class);
				}
			}
		}

		return typeMap;
	}

	/**
	 * Write the table content into the given file
	 * @param file
	 * @param autosizeCols
	 * @throws Exception if the operation has failed
	 */
	protected void writeTableContentToXLSFile(File file, boolean autosizeCols) throws Exception {
		FileOutputStream fileOutputStream = null;
		final FormatDTO userFormat = getFormatPreferences();
		final var decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		final var dateTimeFormat = new SimpleDateFormat(userFormat.getDateTimeFormat());
		final var dateFormat = new SimpleDateFormat(userFormat.getDateFormat());
		var typeMap = new HashMap<String, Class<?>>();
		String itemText = null;
		Cell cell = null;
		Row row = null;

		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			// Create a new sheet
			final XSSFSheet sheet = workbook.createSheet(DEFAULT_SHEET_NAME);
			final CreationHelper createHelper = workbook.getCreationHelper();

			// Define different cell styles
			final CellStyle cellStyleDateTime = workbook.createCellStyle();
			cellStyleDateTime.setDataFormat(createHelper.createDataFormat().getFormat(userFormat.getDateTimeFormat()));

			final CellStyle cellStyleDate = workbook.createCellStyle();
			cellStyleDate.setDataFormat(createHelper.createDataFormat().getFormat(userFormat.getDateFormat()));

			final CellStyle cellStyleDouble = workbook.createCellStyle();
			cellStyleDouble.setDataFormat(createHelper.createDataFormat().getFormat(userFormat.getDecimalFormat()));

			final Font headerFont = workbook.createFont();
			headerFont.setFontHeightInPoints((short) 11);
			headerFont.setBold(true);

			final CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);

			int colCounter = 0;
			int rowCounter = 0;

			// Create the header row
			row = sheet.createRow(rowCounter++);

			// Initialize the export and create the header cells
			if (mode == InitMode.VIA_COLUMN_SORTER) {
				typeMap = initTypes();

				for (final TableColumn col : table.getColumns()) {
					colIdMap.put(colCounter, col.getText());

					cell = row.createCell(colCounter++);
					cell.setCellValue(col.getText());
					cell.setCellStyle(headerCellStyle);
				}
			}
			else if (mode == InitMode.VIA_SEARCH_DTO) {
				typeMap = initTypes();

				for (final SearchFieldDTO field : searchObj.getSearchFields()) {
					if (!field.isVisible())
						continue;

					colIdMap.put(field.getColOrder(), field.getColLabel());

					cell = row.createCell(colCounter++);
					cell.setCellValue(field.getColLabel());
					cell.setCellStyle(headerCellStyle);
				}
			}
			else {
				for (final TableColumn col : table.getColumns()) {
					cell = row.createCell(colCounter++);
					cell.setCellValue(col.getText());
					cell.setCellStyle(headerCellStyle);
				}
			}

			// Save column indexes of date time and date columns in order to expand them later!
			for (final Map.Entry<Integer, String> entry : colIdMap.entrySet()) {
				final Class<?> type = typeMap.get(entry.getValue());

				if (type != null && (type.equals(Calendar.class) || type.equals(Date.class)))
					dateColumnIndexSet.add(entry.getKey());
			}

			for (final TableItem ti : table.getItems()) {
				if ((rowCounter + 1) == SpreadsheetVersion.EXCEL2007.getMaxRows())
					break;

				row = sheet.createRow(rowCounter++);

				// The second row contains the first line with data
				for (int colIndex = 0; colIndex < table.getColumnCount(); colIndex++) {
					cell = row.createCell(colIndex);

					if (ti.getImage(table.getColumnOrder()[colIndex]) != null
							&& ti.getImage(table.getColumnOrder()[colIndex]).equals(ImageCache.getImage(ImageCache.IMG_CHECKED)))
						itemText = TRUE;
					else if (ti.getImage(table.getColumnOrder()[colIndex]) != null
							&& ti.getImage(table.getColumnOrder()[colIndex]).equals(ImageCache.getImage(ImageCache.IMG_UNCHECKED)))
						itemText = FALSE;
					else
						itemText = ti.getText(table.getColumnOrder()[colIndex]);

					if (itemText == null || itemText.isEmpty())
						continue;

					final Class<?> type = typeMap.get(colIdMap.get(colIndex));

					if (type != null && type.equals(Boolean.class))
						cell.setCellValue(itemText.equals(TRUE));
					else if (type != null && type.equals(String.class))
						cell.setCellValue(itemText);
					else if (type != null && type.equals(Calendar.class))
						setCellValue(dateTimeFormat, itemText, cell, cellStyleDateTime);
					else if (type != null && type.equals(Date.class))
						setCellValue(dateFormat, itemText, cell, cellStyleDate);
					else if (type != null && type.equals(Double.class))
						setCellValue(decimalFormat, itemText, cell, cellStyleDouble);
					else if (type != null && type.equals(Long.class))
						setCellValue(itemText, cell);
					else if (type == null)
						cell.setCellValue(itemText);
				}
			}

			if (autosizeCols)
				dateColumnIndexSet.forEach(sheet::autoSizeColumn);

			fileOutputStream = new FileOutputStream(file);
			workbook.write(fileOutputStream);
		}
		catch (final Exception e) {
			logger.error("Error while exporting table data!", e);

			throw e;
		}
		finally {
			try {
				if (fileOutputStream != null)
					fileOutputStream.close();
			}
			catch (final IOException ex) {
				logger.warn("Could not close file output stream!", ex);
			}
		}
	}

	/**
	 * Convert the given string value to a long and set the respective cell
	 * @param value
	 * @param cell
	 */
	private void setCellValue(String value, Cell cell) {
		try {
			cell.setCellValue(Long.parseLong(value));
		}
		catch (final NumberFormatException e) {
			cell.setCellValue(value);
		}
	}

	/**
	 * Convert the given string value to a {@link Number} and set the respective cell
	 * @param decimalFormat
	 * @param value
	 * @param cell
	 * @param cellStyleDouble
	 */
	private void setCellValue(final DecimalFormat decimalFormat, String value, Cell cell, final CellStyle cellStyleDouble) {
		try {
			cell.setCellValue(decimalFormat.parse(value).doubleValue());
			cell.setCellStyle(cellStyleDouble);
		}
		catch (final ParseException e) {
			cell.setCellValue(value);
		}
	}

	/**
	 * Convert the given string value to a {@link Date} and set the respective cell
	 * @param dateFormat
	 * @param value
	 * @param cell
	 * @param cellStyleDateTime
	 */
	private void setCellValue(final SimpleDateFormat dateFormat, String value, Cell cell, final CellStyle cellStyleDateTime) {
		try {
			cell.setCellValue(dateFormat.parse(value));
			cell.setCellStyle(cellStyleDateTime);
		}
		catch (final ParseException e) {
			cell.setCellValue(value);
		}
	}

}
