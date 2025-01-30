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
package net.codecadenza.runtime.richclient.eclipse.search.util;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_COLUMN_SORT_LISTENER_MSG_ERR_COL_NOT_FOUND;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for column sort operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the column sort listener
 */
public abstract class AbstractColumnSortListener<T> implements Listener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	protected FormatDTO userFormat;
	protected DecimalFormat decimalFormat;
	protected SimpleDateFormat dateTimeFormat;
	protected SimpleDateFormat dateFormat;
	private final TableViewer tableViewer;
	private final SearchDTO searchObj;
	private final Table table;

	/**
	 * Constructor
	 * @param tableViewer
	 * @param searchObj
	 * @param userFormat
	 */
	protected AbstractColumnSortListener(TableViewer tableViewer, SearchDTO searchObj, FormatDTO userFormat) {
		this.tableViewer = tableViewer;
		this.table = tableViewer.getTable();
		this.searchObj = searchObj;
		this.userFormat = userFormat;
		this.decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		this.dateTimeFormat = new SimpleDateFormat(userFormat.getDateTimeFormat());
		this.dateFormat = new SimpleDateFormat(userFormat.getDateFormat());
	}

	/**
	 * @param object
	 * @param colIndex
	 * @return the column cell text
	 */
	public abstract String getColText(T object, int colIndex);

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void handleEvent(Event event) {
		try {
			final var col = (TableColumn) event.widget;
			int sortDirection = SWT.UP;

			if (table.getSortDirection() == SWT.UP)
				sortDirection = SWT.DOWN;

			final SearchFieldDTO currentField = searchObj.getSearchFields().stream()
					.filter(field -> field.getColLabel().equals(col.getText())).findFirst().orElse(null);

			if (currentField == null)
				throw new IllegalStateException(getTranslation(ABSTRACT_COLUMN_SORT_LISTENER_MSG_ERR_COL_NOT_FOUND, col.getText()));

			// Take the original column index to find proper data
			final int colIndex = currentField.getOriginalColumnIndex();
			final int sortDirectionMultiplier = (sortDirection == SWT.UP) ? 1 : -1;
			final boolean isDateTime = currentField.isDateTimeFormat();
			final SearchFieldDataTypeEnum type = currentField.getDataType();
			final var data = (List<T>) tableViewer.getInput();

			if (type == SearchFieldDataTypeEnum.BOOLEAN) {
				data.sort((object1, object2) -> {
					Integer value1 = -1;
					Integer value2 = -1;
					final String cellText1 = getColText(object1, colIndex);
					final String cellText2 = getColText(object2, colIndex);

					if (!cellText1.isEmpty()) {
						if (cellText1.equals(Boolean.TRUE.toString()))
							value1 = 1;
						else
							value1 = 0;
					}

					if (!cellText2.isEmpty()) {
						if (cellText2.equals(Boolean.TRUE.toString()))
							value2 = 1;
						else
							value2 = 0;
					}

					return value1.compareTo(value2) * sortDirectionMultiplier;
				});
			}
			else if (type == SearchFieldDataTypeEnum.STRING || type == SearchFieldDataTypeEnum.ENUM
					|| type == SearchFieldDataTypeEnum.CHAR || type == SearchFieldDataTypeEnum.UUID_BINARY
					|| type == SearchFieldDataTypeEnum.UUID_STRING) {
				data.sort((object1, object2) -> {
					final String value1 = getColText(object1, colIndex);
					final String value2 = getColText(object2, colIndex);

					return value1.compareToIgnoreCase(value2) * sortDirectionMultiplier;
				});
			}
			else if (type == SearchFieldDataTypeEnum.FLOAT || type == SearchFieldDataTypeEnum.DOUBLE
					|| type == SearchFieldDataTypeEnum.BIG_DECIMAL) {
				data.sort((object1, object2) -> {
					var value1 = new BigDecimal(Long.MIN_VALUE);
					var value2 = new BigDecimal(Long.MIN_VALUE);
					final String colText1 = getColText(object1, colIndex);
					final String colText2 = getColText(object2, colIndex);

					decimalFormat.setParseBigDecimal(true);

					try {
						if (!colText1.isEmpty())
							value1 = (BigDecimal) decimalFormat.parse(colText1);

						if (!colText2.isEmpty())
							value2 = (BigDecimal) decimalFormat.parse(colText2);

						return value1.compareTo(value2) * sortDirectionMultiplier;
					}
					catch (final ParseException e) {
						logger.warn("Could not parse cell values '{}' and '{}'!", colText1, colText2, e);
					}
					finally {
						decimalFormat.setParseBigDecimal(false);
					}

					return 0;
				});
			}
			else if (currentField.hasTemporalDataType()) {
				data.sort((object1, object2) -> {
					var value1 = new Date(Long.MIN_VALUE);
					var value2 = new Date(Long.MIN_VALUE);
					final String colText1 = getColText(object1, colIndex);
					final String colText2 = getColText(object2, colIndex);

					try {
						if (isDateTime) {
							if (!colText1.isEmpty())
								value1 = dateTimeFormat.parse(colText1);

							if (!colText2.isEmpty())
								value2 = dateTimeFormat.parse(colText2);
						}
						else {
							if (!colText1.isEmpty())
								value1 = dateFormat.parse(colText1);

							if (!colText2.isEmpty())
								value2 = dateFormat.parse(colText2);
						}

						return value1.compareTo(value2) * sortDirectionMultiplier;
					}
					catch (final ParseException e) {
						logger.warn("Could not parse cell values '{}' and '{}'!", colText1, colText2, e);
					}

					return 0;
				});
			}
			else if (type == SearchFieldDataTypeEnum.LONG || type == SearchFieldDataTypeEnum.INTEGER) {
				data.sort((object1, object2) -> {
					Long value1 = Long.MIN_VALUE;
					Long value2 = Long.MIN_VALUE;
					final String colText1 = getColText(object1, colIndex);
					final String colText2 = getColText(object2, colIndex);

					try {
						if (!colText1.isEmpty())
							value1 = Long.parseLong(colText1);

						if (!colText2.isEmpty())
							value2 = Long.parseLong(colText2);

						return value1.compareTo(value2) * sortDirectionMultiplier;
					}
					catch (final NumberFormatException e) {
						logger.warn("Could not parse cell values '{}' and '{}'!", colText1, colText2, e);
					}

					return 0;
				});
			}

			table.setSortColumn(col);
			table.setSortDirection(sortDirection);
			table.showColumn(col);
			tableViewer.setInput(data);
		}
		catch (final Exception e) {
			logger.error("Error while performing sort operation!", e);
		}
	}

}
