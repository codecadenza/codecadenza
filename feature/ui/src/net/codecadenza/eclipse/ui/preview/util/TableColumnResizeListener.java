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
package net.codecadenza.eclipse.ui.preview.util;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.TableColumnField;
import org.eclipse.emf.common.util.EList;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

/**
 * <p>
 * Listener for column resize and move events
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TableColumnResizeListener implements ControlListener {
	private final EList<TableColumnField> columns;

	/**
	 * Constructor
	 * @param panel
	 */
	public TableColumnResizeListener(FormPanel panel) {
		this.columns = panel.getFormTable().getFields();
	}

	/**
	 * Constructor
	 * @param form
	 */
	public TableColumnResizeListener(Form form) {
		this.columns = form.getFormPanels().get(0).getFormTable().getFields();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.events.ControlListener#controlMoved(org.eclipse.swt.events.ControlEvent)
	 */
	@Override
	public void controlMoved(ControlEvent event) {
		final var selectedColumn = (TableColumn) event.widget;
		final Table table = selectedColumn.getParent();
		int index = 1;

		// Iterate over all columns in the order in which they are displayed!
		for (final int i : table.getColumnOrder()) {
			final TableColumn tableColumn = table.getColumn(i);

			// Update the index of the corresponding column in the meta-model
			for (final TableColumnField column : columns)
				if (column.getTitle().equals(tableColumn.getText())) {
					column.setColIndex(index++);
					break;
				}
		}

		// Adjust the column index of all invisible columns!
		for (final TableColumnField column : columns) {
			if (column.isVisible())
				continue;

			column.setColIndex(index++);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.events.ControlListener#controlResized(org.eclipse.swt.events.ControlEvent)
	 */
	@Override
	public void controlResized(ControlEvent event) {
		final TableColumn tableColumn = (TableColumn) event.widget;

		// Iterate over all columns
		for (final TableColumnField column : columns) {
			if (!column.isVisible())
				continue;

			// Change the width of the column that has been resized!
			if (column.getTitle().equals(tableColumn.getText())) {
				column.setWidth(tableColumn.getWidth());
				break;
			}
		}
	}

}
