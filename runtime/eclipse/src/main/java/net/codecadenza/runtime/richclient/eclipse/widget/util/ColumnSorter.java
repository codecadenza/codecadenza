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
package net.codecadenza.runtime.richclient.eclipse.widget.util;

import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Helper class that adds sorting functionality to a grid column
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ColumnSorter implements Listener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final TableColumn col;
	private final ColumnSortType type;
	private final Table table;
	private final int colIndex;
	private DecimalFormat decimalFormatter = new DecimalFormat();
	private DateFormat dateFormatter = new SimpleDateFormat();
	private DateFormat dateTimeFormatter = new SimpleDateFormat();
	private final TableViewer tableViewer;

	/**
	 * Constructor for tables with a table viewer
	 * @param tableViewer the table viewer
	 * @param col the column
	 * @param type the column sort type
	 * @param colIndex the index of the column
	 */
	public ColumnSorter(TableViewer tableViewer, TableColumn col, ColumnSortType type, int colIndex) {
		this(tableViewer, col, type, colIndex, null);
	}

	/**
	 * Constructor for tables with a table viewer
	 * @param tableViewer the table viewer
	 * @param col the column
	 * @param type the column sort type
	 * @param colIndex the index of the column
	 * @param format a string that contains an alternative date or numeric format
	 */
	public ColumnSorter(TableViewer tableViewer, TableColumn col, ColumnSortType type, int colIndex, String format) {
		this.table = col.getParent();
		this.col = col;
		this.type = type;
		this.colIndex = colIndex;
		this.tableViewer = tableViewer;

		if (format != null && !format.isEmpty()) {
			if (type == ColumnSortType.DATE)
				this.dateFormatter = new SimpleDateFormat(format);
			else if (type == ColumnSortType.DATETIME)
				this.dateTimeFormatter = new SimpleDateFormat(format);
			else if (type == ColumnSortType.DECIMAL)
				this.decimalFormatter = new DecimalFormat(format);
		}
	}

	/**
	 * @return the selected sort type
	 */
	public ColumnSortType getSortType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
	 */
	@Override
	public void handleEvent(Event event) {
		// Determine the sort column and the direction
		final TableColumn sortColumn = table.getSortColumn();
		final List<TableItem> data = new ArrayList<>();
		int dir = table.getSortDirection();

		Collections.addAll(data, table.getItems());

		if (sortColumn == col)
			dir = dir == SWT.UP ? SWT.DOWN : SWT.UP;
		else {
			table.setSortColumn(col);
			dir = SWT.UP;
		}

		// Sort the data based on the cell text and the direction
		final int direction = dir;

		if (type == ColumnSortType.STRING) {
			data.sort((a, b) -> {
				final String value1 = a.getText(colIndex).toUpperCase();
				final String value2 = b.getText(colIndex).toUpperCase();

				if (direction == SWT.UP)
					return value1.compareTo(value2);

				return value2.compareTo(value1);
			});
		}
		else if (type == ColumnSortType.INTEGER) {
			data.sort((a, b) -> {
				Long value1 = Long.MIN_VALUE;
				Long value2 = Long.MIN_VALUE;

				try {
					if (!a.getText(colIndex).isEmpty())
						value1 = Long.parseLong(a.getText(colIndex));

					if (!b.getText(colIndex).isEmpty())
						value2 = Long.parseLong(b.getText(colIndex));
				}
				catch (final NumberFormatException e) {
					logger.warn("Could not parse cell values '{}' and '{}'!", a.getText(colIndex), b.getText(colIndex), e);

					return 0;
				}

				if (direction == SWT.UP)
					return value1.compareTo(value2);

				return value2.compareTo(value1);
			});
		}
		else if (type == ColumnSortType.DECIMAL) {
			data.sort((a, b) -> {
				var value1 = new BigDecimal(Long.MIN_VALUE);
				var value2 = new BigDecimal(Long.MIN_VALUE);

				decimalFormatter.setParseBigDecimal(true);

				try {
					if (!a.getText(colIndex).isEmpty())
						value1 = (BigDecimal) decimalFormatter.parse(a.getText(colIndex));

					if (!b.getText(colIndex).isEmpty())
						value2 = (BigDecimal) decimalFormatter.parse(b.getText(colIndex));
				}
				catch (final Exception e) {
					logger.warn("Could not parse cell values '{}' and '{}'!", a.getText(colIndex), b.getText(colIndex), e);

					return 0;
				}
				finally {
					decimalFormatter.setParseBigDecimal(false);
				}

				if (direction == SWT.UP)
					return value1.compareTo(value2);

				return value2.compareTo(value1);
			});
		}
		else if (type == ColumnSortType.DATE) {
			data.sort((a, b) -> {
				Long value1 = Long.MIN_VALUE;
				Long value2 = Long.MIN_VALUE;

				try {
					if (!a.getText(colIndex).isEmpty())
						value1 = dateFormatter.parse(a.getText(colIndex)).getTime();

					if (!b.getText(colIndex).isEmpty())
						value2 = dateFormatter.parse(b.getText(colIndex)).getTime();
				}
				catch (final ParseException e) {
					logger.warn("Could not parse cell values '{}' and '{}'!", a.getText(colIndex), b.getText(colIndex), e);

					return 0;
				}

				if (direction == SWT.UP)
					return value1.compareTo(value2);

				return value2.compareTo(value1);
			});
		}
		else if (type == ColumnSortType.DATETIME) {
			data.sort((a, b) -> {
				Long value1 = Long.MIN_VALUE;
				Long value2 = Long.MIN_VALUE;

				try {
					if (!a.getText(colIndex).isEmpty())
						value1 = dateTimeFormatter.parse(a.getText(colIndex)).getTime();

					if (!b.getText(colIndex).isEmpty())
						value2 = dateTimeFormatter.parse(b.getText(colIndex)).getTime();
				}
				catch (final ParseException e) {
					logger.warn("Could not parse cell values '{}' and '{}'!", a.getText(colIndex), b.getText(colIndex), e);

					return 0;
				}

				if (direction == SWT.UP)
					return value1.compareTo(value2);

				return value2.compareTo(value1);
			});
		}
		else if (type == ColumnSortType.BOOLEAN) {
			data.sort((a, b) -> {
				Short val1 = Short.MIN_VALUE;
				Short val2 = Short.MIN_VALUE;

				if (a.getImage(colIndex) != null) {
					if (a.getImage(colIndex).equals(ImageCache.getImage(ImageCache.IMG_CHECKED)))
						val1 = 1;
					else
						val1 = 0;
				}

				if (b.getImage(colIndex) != null) {
					if (b.getImage(colIndex).equals(ImageCache.getImage(ImageCache.IMG_CHECKED)))
						val2 = 1;
					else
						val2 = 0;
				}

				if (direction == SWT.UP)
					return val1.compareTo(val2);

				return val2.compareTo(val1);
			});
		}

		final var strings = new String[data.size()][table.getColumnCount()];
		final var images = new Image[data.size()][table.getColumnCount()];
		final var objectArray = new Object[data.size()];
		final var objectList = new ArrayList<>();
		final int dataSize = data.size();

		for (int i = 0; i < dataSize; i++) {
			objectArray[i] = data.get(i).getData();
			objectList.add(data.get(i).getData());

			for (int j = 0; j < table.getColumnCount(); j++) {
				strings[i][j] = data.get(i).getText(j);
				images[i][j] = data.get(i).getImage(j);
			}
		}

		// Refresh the table
		if (tableViewer.getInput() != null
				&& (tableViewer.getInput() instanceof List || tableViewer.getInput() instanceof Object[])) {
			if (tableViewer.getInput() instanceof List)
				tableViewer.setInput(objectList);
			else
				tableViewer.setInput(objectArray);
		}
		else {
			table.removeAll();

			for (int i = 0; i < dataSize; i++) {
				final var tableItem = new TableItem(table, SWT.NONE);
				tableItem.setText(strings[i]);
				tableItem.setImage(images[i]);
				tableItem.setData(objectArray[i]);
			}
		}

		table.setSortDirection(dir);
		table.showColumn(col);
	}

}
