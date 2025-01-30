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

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

/**
 * <p>
 * Class that provides common clipboard operations for a given {@link org.eclipse.jface.viewers.TableViewer}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TableViewerClipboardHelper {
	protected TableViewer tableViewer;
	protected Table table;
	protected Clipboard clipboard;
	protected String selectedCellContent;
	protected int selectedColumnIndex = -1;

	/**
	 * Constructor
	 * @param display
	 * @param tableViewer
	 */
	public TableViewerClipboardHelper(Display display, final TableViewer tableViewer) {
		this.tableViewer = tableViewer;
		this.table = tableViewer.getTable();
		clipboard = new Clipboard(display);

		table.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDown(final MouseEvent e) {
				final var p = new Point(e.x, e.y);
				final TableItem item = table.getItem(p);

				if (item == null)
					return;

				for (int i = 0; i < table.getColumnCount(); i++) {
					final Rectangle rect = item.getBounds(i);

					if (rect.contains(p)) {
						selectedCellContent = item.getText(i);
						selectedColumnIndex = i;
						break;
					}
				}
			}
		});
	}

	/**
	 * Replace all carriage return and line feed characters by a blank
	 * @param input
	 * @return the input string without carriage return and line feed characters
	 */
	protected String replaceCRLFCharacters(String input) {
		return input.replace("\r\n", " ").replace("\r", " ").replace("\n", " ").trim();
	}

	/**
	 * @return the selection text
	 */
	public String getSelectedCellContent() {
		return selectedCellContent;
	}

	/**
	 * Fill the clipboard with the selected cell content
	 */
	public void copySelectionToClipboard() {
		copySelectionToClipboard(replaceCRLFCharacters(selectedCellContent));
	}

	/**
	 * Fill the clipboard with the given content
	 * @param content
	 */
	public void copySelectionToClipboard(String content) {
		clipboard.clearContents();

		if (content != null && !content.isEmpty())
			clipboard.setContents(new Object[] { content }, new Transfer[] { TextTransfer.getInstance() });
	}

	/**
	 * Copy the row content to the clipboard
	 */
	public void copyRowContentToClipboard() {
		if (table.getSelectionCount() < 1)
			return;

		final var s = new StringBuilder();
		final int rowIndex = table.getSelectionIndex();

		for (int i = 0; i < table.getColumnCount(); i++) {
			s.append(replaceCRLFCharacters(table.getItem(rowIndex).getText(table.getColumnOrder()[i])));
			s.append("\t");
		}

		copySelectionToClipboard(s.toString());
	}

	/**
	 * Copy the column content to the clipboard
	 */
	public void copyColumnContentToClipboard() {
		if (selectedColumnIndex == -1)
			return;

		final var s = new StringBuilder();
		final String lineSeparator = System.lineSeparator();

		for (int i = 0; i < table.getItemCount(); i++) {
			s.append(replaceCRLFCharacters(table.getItem(i).getText(table.getColumnOrder()[selectedColumnIndex])));
			s.append(lineSeparator);
		}

		copySelectionToClipboard(s.toString());
	}

	/**
	 * Dispose the clipboard
	 */
	public void dispose() {
		clipboard.dispose();
	}

}
