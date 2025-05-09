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
package net.codecadenza.eclipse.testing.dialog.client;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for editing a form field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditFormFieldDialog extends AbstractDialog {
	private static final String FIELD_TYPE_LABEL = "LABEL";
	private static final String LABEL_SUFFIX = " of this object";
	private static final String LBL_LABEL = "Label:";
	private static final String LBL_READONLY = "Readonly:";
	private static final String LBL_TYPE = "Type:";
	private static final String SHELL_TITLE = "Edit form field";

	/**
	 * Constructor
	 * @param bot
	 */
	public EditFormFieldDialog(SWTWorkbenchBot bot) {
		super(bot, SHELL_TITLE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var txtLabel = bot.textWithLabel(LBL_LABEL);
		final var chkReadonly = bot.checkBoxWithLabel(LBL_READONLY);
		final String text = txtLabel.getText();

		txtLabel.setText(text + LABEL_SUFFIX);

		if (!chkReadonly.isEnabled())
			bot.comboBoxWithLabel(LBL_TYPE).setSelection(FIELD_TYPE_LABEL);

		bot.button(CMD_OK).click();
	}

}
