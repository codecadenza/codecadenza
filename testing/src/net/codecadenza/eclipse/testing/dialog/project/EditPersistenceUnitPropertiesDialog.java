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
package net.codecadenza.eclipse.testing.dialog.project;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for editing the persistence unit properties of a project
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditPersistenceUnitPropertiesDialog extends AbstractDialog {
	private static final String INITIAL_PROPERTY_VALUE = "false";
	private static final String PROPERTY_NAME = "codecadenca.test";
	private static final String PROPERTY_VALUE = "true";
	private static final String SHELL_TITLE = "Edit persistence unit properties";
	private static final Logger log = LoggerFactory.getLogger(EditPersistenceUnitPropertiesDialog.class);

	/**
	 * Constructor
	 * @param bot
	 * @param operationMode
	 */
	public EditPersistenceUnitPropertiesDialog(SWTWorkbenchBot bot, OperationMode operationMode) {
		super(bot, SHELL_TITLE, operationMode);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		if (operationMode == OperationMode.ADD) {
			log.debug("Create persistence unit property '{}'", PROPERTY_NAME);
			bot.button(CMD_ADD).click();
			new CreatePersistenceUnitPropertyDialog(bot, PROPERTY_NAME, INITIAL_PROPERTY_VALUE).enterData();
		}
		else if (operationMode == OperationMode.EDIT) {
			log.debug("Edit persistence unit property '{}'", PROPERTY_NAME);
			bot.table().getTableItem(PROPERTY_NAME).doubleClick();
			new EditPersistenceUnitPropertyDialog(bot, PROPERTY_NAME, PROPERTY_VALUE).enterData();
		}
		else if (operationMode == OperationMode.REMOVE) {
			log.debug("Delete persistence unit property '{}'", PROPERTY_NAME);
			bot.table().getTableItem(PROPERTY_NAME).select();
			bot.button(CMD_REMOVE);
		}

		bot.button(CMD_CANCEL).click();
	}

}
