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
package net.codecadenza.runtime.richclient.swing.widget.actions;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.COPY_CELL_TO_CLIPBOARD_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.COPY_CELL_TO_CLIPBOARD_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import net.codecadenza.runtime.richclient.swing.util.type.Pair;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable;

/**
 * <p>
 * Action to copy the cell content to the clipboard
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CopyCellToClipboardAction extends AbstractAction {
	private static final long serialVersionUID = -9038240621238412993L;

	private final JDataTable<?> dataTable;

	/**
	 * Constructor
	 * @param dataTable
	 */
	public CopyCellToClipboardAction(JDataTable<?> dataTable) {
		super(getTranslation(COPY_CELL_TO_CLIPBOARD_ACTION_NAME));

		putValue(SHORT_DESCRIPTION, getTranslation(COPY_CELL_TO_CLIPBOARD_ACTION_SHORT_DESC));
		this.dataTable = dataTable;
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		final Pair<Integer, Integer> cellPos = dataTable.getSelectedCell();

		if (cellPos == null)
			return;

		final Clipboard c = Toolkit.getDefaultToolkit().getSystemClipboard();
		final String cellValue = dataTable.getTextValue(cellPos);
		final var data = new StringSelection(cellValue);

		c.setContents(data, data);
	}

}
