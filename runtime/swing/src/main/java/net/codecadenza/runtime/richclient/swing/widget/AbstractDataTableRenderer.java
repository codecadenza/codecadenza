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
package net.codecadenza.runtime.richclient.swing.widget;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_DATA_TABLE_RENDERER_TABLE_NOT_SET;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.Icon;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

/**
 * <p>
 * Data table renderer
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the table renderer
 */
public abstract class AbstractDataTableRenderer<T> implements TableCellRenderer {
	private JDataTable<?> table;

	/**
	 * @param table
	 */
	public void setTable(JDataTable<?> table) {
		this.table = table;
	}

	/**
	 * @return the table of this renderer
	 */
	@SuppressWarnings("unchecked")
	protected JDataTable<T> getTable() {
		if (table == null)
			throw new IllegalStateException(getTranslation(ABSTRACT_DATA_TABLE_RENDERER_TABLE_NOT_SET));

		return (JDataTable<T>) table;
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean,
	 * boolean, int, int)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Component getTableCellRendererComponent(JTable table, Object object, boolean isSelected, boolean hasFocus, int row,
			int column) {
		final T value = (T) object;

		// Determine the column index
		column = getTable().getOriginalIndexMap().get(column);

		// Reuse the DefaultTableCellRenderer's selection behavior
		final var cellRenderer = new DefaultTableCellRenderer();
		cellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
		cellRenderer.setText(getCellText(value, column));
		cellRenderer.setIcon(getCellImage(value, column));

		final Font font = getCellFont(value, column);

		if (font != null)
			cellRenderer.setFont(font);

		if (!isSelected) {
			final Color fg = getCellForeground(value, column);

			if (fg != null)
				cellRenderer.setForeground(fg);

			final Color bg = getCellBackground(value, column);

			if (bg != null)
				cellRenderer.setBackground(bg);
		}

		return cellRenderer;
	}

	/**
	 * @param value
	 * @param column
	 * @return the text of a cell
	 */
	@SuppressWarnings("unchecked")
	public String getTableCellText(Object value, int column) {
		final String text = getCellExportText((T) value, column);

		if (text == null)
			return "";

		return text;
	}

	/**
	 * @param value
	 * @param column
	 * @return the cell text
	 */
	protected abstract String getCellText(T value, int column);

	/**
	 * @param value
	 * @param column
	 * @return the cell image
	 */
	protected abstract Icon getCellImage(T value, int column);

	/**
	 * @param value
	 * @param column
	 * @return the cell font
	 */
	protected abstract Font getCellFont(T value, int column);

	/**
	 * @param value
	 * @param column
	 * @return the cell foreground color
	 */
	protected abstract Color getCellForeground(T value, int column);

	/**
	 * @param value
	 * @param column
	 * @return the cell background color
	 */
	protected abstract Color getCellBackground(T value, int column);

	/**
	 * @param value
	 * @param column
	 * @return the cell value
	 */
	protected abstract Comparable<?> getCellValue(T value, int column);

	/**
	 * @param element
	 * @param columnIndex
	 * @return the text to be used in export operations
	 */
	protected abstract String getCellExportText(T element, int columnIndex);

}
