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
package net.codecadenza.runtime.webclient.vaadin.util;

import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.function.ValueProvider;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.codecadenza.runtime.exchange.DataExportException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class that generates a Microsoft Excel export file based on the data of a {@link Grid}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data that is used in the grid
 */
public class XLSXExportUtility<T> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String DEFAULT_SHEET_NAME = "Sheet 1";
	private static final String NUMBER_FORMAT = "###,###,##0.00";
	private static final String DATE_FORMAT = "dd.MM.yyyy HH:mm:ss";

	private final Grid<T> grid;
	private final List<T> data;
	private final Map<String, ValueProvider<T, ?>> columnValueProviders;
	private SXSSFWorkbook workbook;
	private Sheet sheet;
	private CellStyle cellStyleDateTime;
	private CellStyle cellStyleNumber;

	/**
	 * Constructor
	 * @param grid
	 * @param data
	 * @param columnValueProviders
	 */
	public XLSXExportUtility(Grid<T> grid, List<T> data, Map<String, ValueProvider<T, ?>> columnValueProviders) {
		this.data = data;
		this.grid = grid;
		this.columnValueProviders = columnValueProviders;
	}

	/**
	 * @return a stream that contains the generated content
	 * @throws DataExportException if the data export operation has failed
	 */
	public InputStream createResource() {
		try {
			logger.debug("Create resource for grid data export");

			return writeXLSXToStream();
		}
		catch (final Exception e) {
			logger.error("Error while exporting grid data!", e);

			throw new DataExportException(e);
		}
	}

	/**
	 * Write the grid data to an {@link InputStream}
	 * @return a stream that contains the generated content
	 * @throws IOException if the content could not be written to the {@link InputStream}
	 */
	private InputStream writeXLSXToStream() throws IOException {
		Row row = null;
		int colIndex = 0;
		int rowIndex = 1;

		initWorkbook();

		try (final var outputStream = new ByteArrayOutputStream()) {
			createHeader();

			for (final T item : data) {
				// Create a new row for every data object of the grid
				row = sheet.createRow(rowIndex++);
				colIndex = 0;

				logger.debug("Create row {}", rowIndex);

				for (final Column<T> column : grid.getColumns()) {
					if (!column.isVisible() || !columnValueProviders.containsKey(column.getKey()))
						continue;

					// Get the value of a specific cell
					final Object value = columnValueProviders.get(column.getKey()).apply(item);

					createCell(row, value, colIndex++);
				}
			}

			logger.debug("Cell generation finished!");

			workbook.write(outputStream);

			return new ByteArrayInputStream(outputStream.toByteArray());
		}
		finally {
			workbook.close();
		}
	}

	/**
	 * Initialize the workbook and other POI fields
	 */
	private void initWorkbook() {
		workbook = new SXSSFWorkbook();

		// Create a new sheet
		sheet = workbook.createSheet(DEFAULT_SHEET_NAME);

		final CreationHelper createHelper = workbook.getCreationHelper();

		cellStyleDateTime = workbook.createCellStyle();
		cellStyleDateTime.setDataFormat(createHelper.createDataFormat().getFormat(DATE_FORMAT));

		cellStyleNumber = workbook.createCellStyle();
		cellStyleNumber.setDataFormat(createHelper.createDataFormat().getFormat(NUMBER_FORMAT));
	}

	/**
	 * Create the header
	 */
	private void createHeader() {
		logger.debug("Create header");

		final Font headerFont = workbook.createFont();
		headerFont.setFontHeightInPoints((short) 11);
		headerFont.setBold(true);

		final CellStyle headerCellStyle = workbook.createCellStyle();
		headerCellStyle.setFont(headerFont);

		int colIndex = 0;

		// Create the header row
		final Row row = sheet.createRow(0);

		// Create the header cells
		for (final Column<T> column : grid.getColumns()) {
			if (!column.isVisible() || !columnValueProviders.containsKey(column.getKey()))
				continue;

			final Cell cell = row.createCell(colIndex++);
			cell.setCellValue(column.getKey());
			cell.setCellStyle(headerCellStyle);
		}
	}

	/**
	 * Create a cell in the given row at the specified column index
	 * @param row
	 * @param value
	 * @param colIndex
	 */
	private void createCell(final Row row, final Object value, final int colIndex) {
		final Cell cell = row.createCell(colIndex);

		if (value == null)
			return;

		final Class<?> type = value.getClass();

		if (type.equals(Boolean.class))
			cell.setCellValue((Boolean) value);
		else if (type.equals(String.class))
			cell.setCellValue((String) value);
		else if (value instanceof final Enum<?> enumeration)
			cell.setCellValue(enumeration.name());
		else if (type.equals(Character.class))
			cell.setCellValue((Character) value);
		else if (type.equals(Long.class))
			cell.setCellValue((Long) value);
		else if (type.equals(Integer.class))
			cell.setCellValue((Integer) value);
		else if (type.equals(Calendar.class) || type.equals(GregorianCalendar.class)) {
			cell.setCellValue((Calendar) value);
			cell.setCellStyle(cellStyleDateTime);
		}
		else if (type.equals(Date.class) || type.equals(Timestamp.class) || type.equals(java.sql.Date.class)) {
			cell.setCellValue((Date) value);
			cell.setCellStyle(cellStyleDateTime);
		}
		else if (type.equals(LocalDate.class)) {
			final var localDate = (LocalDate) value;

			cell.setCellValue(Date.from(localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()));
			cell.setCellStyle(cellStyleDateTime);
		}
		else if (type.equals(LocalDateTime.class)) {
			final var localDateTime = (LocalDateTime) value;

			cell.setCellValue(Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant()));
			cell.setCellStyle(cellStyleDateTime);
		}
		else if (type.equals(Double.class)) {
			cell.setCellValue((Double) value);

			cell.setCellStyle(cellStyleNumber);
		}
		else if (type.equals(Float.class)) {
			cell.setCellValue((Float) value);

			cell.setCellStyle(cellStyleNumber);
		}
		else if (type.equals(BigDecimal.class)) {
			final var decimalValue = (BigDecimal) value;
			cell.setCellValue(decimalValue.doubleValue());
		}
		else if (type.equals(UUID.class)) {
			final var uuidValue = (UUID) value;
			cell.setCellValue(uuidValue.toString());
		}
	}

}
