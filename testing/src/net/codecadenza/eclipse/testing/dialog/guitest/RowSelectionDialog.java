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
package net.codecadenza.eclipse.testing.dialog.guitest;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for adding a new row selection action
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RowSelectionDialog extends AbstractDialog {
	private static final String DEFAULT_ROW_VALUE = "1";
	private static final String LBL_DOUBLE_CLICK = "performing a double click";
	private static final String LBL_RADIO_ROW_INDEX = "a row index";
	private static final String LBL_RADIO_CELL_VALUE = "a cell value";
	private static final String LBL_ROW_VALUE = "with value";
	private static final String SHELL_TITLE = "Create new row selection action";

	private final boolean useRowIndex;

	/**
	 * Constructor
	 * @param bot
	 * @param useRowIndex
	 */
	public RowSelectionDialog(SWTWorkbenchBot bot, boolean useRowIndex) {
		super(bot, SHELL_TITLE);

		this.useRowIndex = useRowIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		if (useRowIndex) {
			bot.radio(LBL_RADIO_ROW_INDEX).click();
			bot.textWithLabel(LBL_ROW_VALUE).setText(DEFAULT_ROW_VALUE);
		}
		else {
			bot.radio(LBL_RADIO_CELL_VALUE).click();
			bot.textWithLabel(LBL_ROW_VALUE).setText(DEFAULT_ROW_VALUE);
			bot.radio(LBL_DOUBLE_CLICK).click();

			// Search for value in all available pages
			bot.checkBox().select();
		}

		bot.button(CMD_OK).click();
	}

}
