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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.COPY_ROW_TO_CLIPBOARD_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.COPY_ROW_TO_CLIPBOARD_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.util.List;
import javax.swing.AbstractAction;
import net.codecadenza.runtime.richclient.swing.util.type.Joiner;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable;

/**
 * <p>
 * Action to copy the row content to the clipboard
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CopyRowToClipboardAction extends AbstractAction {
	private static final long serialVersionUID = -9038240621238412993L;

	private final JDataTable<?> dataTable;

	/**
	 * Constructor
	 * @param dataTable
	 */
	public CopyRowToClipboardAction(JDataTable<?> dataTable) {
		super(getTranslation(COPY_ROW_TO_CLIPBOARD_ACTION_NAME));

		putValue(SHORT_DESCRIPTION, getTranslation(COPY_ROW_TO_CLIPBOARD_ACTION_SHORT_DESC));
		this.dataTable = dataTable;
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		final List<String> row = dataTable.getSelectedRowText();

		if (row.isEmpty())
			return;

		final Clipboard c = Toolkit.getDefaultToolkit().getSystemClipboard();
		final var data = new StringSelection(Joiner.on('\t').join(row, true));

		c.setContents(data, data);
	}

}
