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
package net.codecadenza.runtime.richclient.javafx.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.UUID;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import org.apache.poi.ss.SpreadsheetVersion;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class that generates a Microsoft Excel export file based on data provided by a {@link TableView}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XLSXExportUtility {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String DEFAULT_SHEET_NAME = "Sheet 1";
	private static final String TEMP_FILE_PREFIX = "tmp";
	private static final String TEMP_FILE_SUFFIX = ".xlsx";

	private final HashSet<Integer> dateColumnIndexSet = new HashSet<>();
	private final TableView<?> tableView;
	protected FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected SimpleDateFormat dateTimeFormat = new SimpleDateFormat(userFormat.getDateTimeFormat());
	protected SimpleDateFormat dateFormat = new SimpleDateFormat(userFormat.getDateFormat());
	protected DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());

	/**
	 * Constructor
	 * @param tableView
	 */
	public XLSXExportUtility(TableView<?> tableView) {
		this.tableView = tableView;
	}

	/**
	 * @return a hash map of all column headers with their data types
	 */
	private HashMap<String, Class<?>> initTypes() {
		final var typeMap = new HashMap<String, Class<?>>();

		for (final TableColumn<?, ?> col : tableView.getColumns()) {
			// We are not interested in invisible columns
			if (!col.isVisible())
				continue;

			// By default, every cell should be mapped to a String!
			typeMap.put(col.getText(), String.class);

			final var type = (SearchFieldDataTypeEnum) col.getUserData();

			if (type == SearchFieldDataTypeEnum.BOOLEAN)
				typeMap.put(col.getText(), Boolean.class);
			else if (type == SearchFieldDataTypeEnum.CHAR)
				typeMap.put(col.getText(), Character.class);
			else if (type == SearchFieldDataTypeEnum.DOUBLE || type == SearchFieldDataTypeEnum.FLOAT)
				typeMap.put(col.getText(), Double.class);
			else if (type == SearchFieldDataTypeEnum.BIG_DECIMAL)
				typeMap.put(col.getText(), BigDecimal.class);
			else if (type == SearchFieldDataTypeEnum.ENUM)
				typeMap.put(col.getText(), Enum.class);
			else if (type == SearchFieldDataTypeEnum.INTEGER || type == SearchFieldDataTypeEnum.LONG)
				typeMap.put(col.getText(), Long.class);
			else if (type == SearchFieldDataTypeEnum.GREGORIAN_CALENDAR)
				typeMap.put(col.getText(), Calendar.class);
			else if (type == SearchFieldDataTypeEnum.DATE)
				typeMap.put(col.getText(), Date.class);
			else if (type == SearchFieldDataTypeEnum.LOCAL_DATE)
				typeMap.put(col.getText(), LocalDate.class);
			else if (type == SearchFieldDataTypeEnum.LOCAL_DATE_TIME)
				typeMap.put(col.getText(), LocalDateTime.class);
			else if (type == SearchFieldDataTypeEnum.UUID_BINARY || type == SearchFieldDataTypeEnum.UUID_STRING)
				typeMap.put(col.getText(), UUID.class);
		}

		return typeMap;
	}

	/**
	 * Export the content and save it in a temporary file
	 * @return the generated file
	 * @throws IOException if the content could not be written to this file
	 * @throws FileNotFoundException if the file doesn't exist
	 * @throws ParseException if either date or numeric values could not be parsed
	 */
	public File exportToTempFile() throws IOException, ParseException {
		// Create the temporary file
		final File tempFile = Files.createTempFile(TEMP_FILE_PREFIX, TEMP_FILE_SUFFIX).toFile();

		// Write the content to the file
		writeContentToXLSXFile(tempFile);

		return tempFile;
	}

	/**
	 * Export the content and save it to the selected file
	 * @param file
	 * @throws IOException if the content could not be written to this file
	 * @throws FileNotFoundException if the file doesn't exist
	 * @throws ParseException if either date or numeric values could not be parsed
	 */
	public void exportToFile(File file) throws IOException, ParseException {
		// Write the content to the file
		writeContentToXLSXFile(file);
	}

	/**
	 * Write the content to the file
	 * @param file
	 * @throws IOException if the content could not be written to this file
	 * @throws FileNotFoundException if the file doesn't exist
	 * @throws ParseException if either date or numeric values could not be parsed
	 */
	public void writeContentToXLSXFile(File file) throws IOException, ParseException {
		FileOutputStream fileOutputStream = null;
		HashMap<String, Class<?>> typeMap;
		Cell cell = null;
		Row row = null;

		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			// Create a new sheet
			final XSSFSheet sheet = workbook.createSheet(DEFAULT_SHEET_NAME);
			final CreationHelper createHelper = workbook.getCreationHelper();

			// Define different cell styles
			final CellStyle cellStyleDateTime = workbook.createCellStyle();
			cellStyleDateTime.setDataFormat(createHelper.createDataFormat().getFormat("dd.MM.yyyy HH:mm:ss"));

			final CellStyle cellStyleDate = workbook.createCellStyle();
			cellStyleDate.setDataFormat(createHelper.createDataFormat().getFormat("dd.MM.yyyy"));

			final CellStyle cellStyleDouble = workbook.createCellStyle();
			cellStyleDouble.setDataFormat(createHelper.createDataFormat().getFormat("###,###,##0.00"));

			final Font headerFont = workbook.createFont();
			headerFont.setFontHeightInPoints((short) 11);
			headerFont.setBold(true);

			final CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);

			int colCounter = 0;
			int rowCounter = 0;

			// Create the header row
			row = sheet.createRow(rowCounter++);

			typeMap = initTypes();

			for (final TableColumn<?, ?> col : tableView.getColumns()) {
				if (!col.isVisible())
					continue;

				cell = row.createCell(colCounter++);
				cell.setCellValue(col.getText());
				cell.setCellStyle(headerCellStyle);

				final Class<?> type = typeMap.get(col.getText());

				if (type == null)
					throw new IllegalStateException("No type found for column index \"" + (colCounter - 1) + "\"!");

				// Save indexes of columns that contain date values in order to expand them later!
				if (type.equals(Date.class) || type.equals(Calendar.class) || type.equals(LocalDate.class)
						|| type.equals(LocalDateTime.class))
					dateColumnIndexSet.add(colCounter - 1);
			}

			for (int rowIndex = 0; rowIndex < tableView.getItems().size(); rowIndex++) {
				if ((rowIndex + 1) == SpreadsheetVersion.EXCEL2007.getMaxRows())
					break;

				row = sheet.createRow(rowCounter++);
				int colIndex = 0;

				for (final TableColumn<?, ?> col : tableView.getColumns()) {
					if (!col.isVisible())
						continue;

					cell = row.createCell(colIndex++);

					// Get the value for a specific cell
					final Object cellData = col.getCellData(rowIndex);

					if (!(cellData instanceof final String value))
						continue;

					if (value.isEmpty())
						continue;

					final Class<?> type = typeMap.get(col.getText());

					if (type.equals(Boolean.class))
						cell.setCellValue(Boolean.parseBoolean(value));
					else if (type.equals(String.class) || type.equals(Enum.class) || type.equals(Character.class)
							|| type.equals(UUID.class))
						cell.setCellValue(value);
					else if (type.equals(Long.class))
						cell.setCellValue(Long.parseLong(value));
					else if (type.equals(Date.class) || type.equals(Calendar.class)) {
						try {
							// At this point we cannot be sure about the format being used!
							cell.setCellValue(dateTimeFormat.parse(value));

							cell.setCellStyle(cellStyleDateTime);
						}
						catch (final ParseException _) {
							// If parsing fails we will try again with a different format!
							cell.setCellValue(dateFormat.parse(value));

							cell.setCellStyle(cellStyleDate);
						}
					}
					else if (type.equals(Double.class) || type.equals(BigDecimal.class)) {
						cell.setCellValue(decimalFormat.parse(value).doubleValue());
						cell.setCellStyle(cellStyleDouble);
					}
					else if (type.equals(LocalDate.class)) {
						cell.setCellValue(dateFormat.parse(value));
						cell.setCellStyle(cellStyleDate);
					}
					else if (type.equals(LocalDateTime.class)) {
						cell.setCellValue(dateTimeFormat.parse(value));
						cell.setCellStyle(cellStyleDateTime);
					}
				}
			}

			dateColumnIndexSet.forEach(sheet::autoSizeColumn);

			fileOutputStream = new FileOutputStream(file);
			workbook.write(fileOutputStream);
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

}
