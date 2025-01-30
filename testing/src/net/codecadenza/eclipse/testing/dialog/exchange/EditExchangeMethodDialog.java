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
package net.codecadenza.eclipse.testing.dialog.exchange;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.ContentType;
import net.codecadenza.eclipse.testing.domain.DataExchangeMethod;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

/**
 * <p>
 * Dialog for editing a data exchange object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditExchangeMethodDialog extends AbstractDialog {
	private static final String LBL_MAPPING_STRUCTURE = "Mapping (file) structure:";
	private static final String MNU_ADD_ATTRIBUTE = "Add attribute";
	private static final String MNU_ADD_MAPPED_ELEMENT = "Add mapped element";
	private static final String MNU_ADD_UNMAPPED_ELEMENT = "Add unmapped element";
	private static final String MNU_DELETE_PARAMETER = "Delete parameter";
	private static final String TAB_ITEM_FILTER_PARAMETERS = "Filter parameters";
	private static final String SHELL_TITLE = "Edit data exchange method";
	private static final String SHELL_TITLE_DELETE_FILTER_PARAMETER = "Delete filter parameter";

	private final DataExchangeMethod exchangeMethod;

	/**
	 * Constructor
	 * @param bot
	 * @param exchangeMethod
	 */
	public EditExchangeMethodDialog(SWTWorkbenchBot bot, DataExchangeMethod exchangeMethod) {
		super(bot, SHELL_TITLE);

		this.exchangeMethod = exchangeMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var treeViewStructure = bot.treeWithLabel(LBL_MAPPING_STRUCTURE);
		final var rootItem = treeViewStructure.getAllItems()[0];
		String parentElementName = rootItem.getText();
		SWTBotTreeItem elementsTreeItem = null;
		SWTBotTreeItem attributesTreeItem = null;

		if (exchangeMethod.getContentType() == ContentType.XML) {
			if (exchangeMethod.isProcessSingleObject()) {
				elementsTreeItem = rootItem.getItems()[0];
				attributesTreeItem = rootItem.getItems()[1];
			}
			else {
				elementsTreeItem = rootItem.getItems()[0].getItems()[0];
				attributesTreeItem = rootItem.getItems()[0].getItems()[1];
				parentElementName = rootItem.getItems()[0].getText();
			}
		}
		else if (exchangeMethod.getContentType() == ContentType.JSON)
			attributesTreeItem = rootItem.getItems()[1];
		else
			attributesTreeItem = rootItem.getItems()[0];

		// Create new data exchange attributes
		attributesTreeItem.contextMenu(MNU_ADD_ATTRIBUTE).click();
		new CreateExchangeAttributeDialog(bot, parentElementName, true).enterData();
		activateShellWithTitle(SHELL_TITLE);

		attributesTreeItem.contextMenu(MNU_ADD_ATTRIBUTE).click();
		new CreateExchangeAttributeDialog(bot, parentElementName, false).enterData();
		activateShellWithTitle(SHELL_TITLE);

		if (elementsTreeItem != null) {
			// Create new data exchange elements
			elementsTreeItem.contextMenu(MNU_ADD_MAPPED_ELEMENT).click();
			new CreateExchangeElementDialog(bot, true).enterData();
			activateShellWithTitle(SHELL_TITLE);

			elementsTreeItem.contextMenu(MNU_ADD_UNMAPPED_ELEMENT).click();
			new CreateExchangeElementDialog(bot, false).enterData();
			activateShellWithTitle(SHELL_TITLE);
		}

		// Open the dialog for editing the root data exchange element
		rootItem.doubleClick();
		new EditExchangeElementDialog(bot, rootItem.getText()).enterData();
		activateShellWithTitle(SHELL_TITLE);

		// Open the edit dialog for every data exchange attribute
		for (final var attributeItem : attributesTreeItem.getItems()) {
			attributeItem.doubleClick();
			new EditExchangeAttributeDialog(bot, attributeItem.getText()).enterData();
			activateShellWithTitle(SHELL_TITLE);
		}

		if (!exchangeMethod.isProcessSingleObject() && !exchangeMethod.isImportData()) {
			// Remove the filter parameter
			bot.tabItem(TAB_ITEM_FILTER_PARAMETERS).activate();

			final var tableFilterParameters = bot.table();
			tableFilterParameters.getTableItem(0).select();
			tableFilterParameters.contextMenu(MNU_DELETE_PARAMETER).click();

			final var dlgDeleteParam = activateShellWithTitle(SHELL_TITLE_DELETE_FILTER_PARAMETER);
			dlgDeleteParam.bot().button(CMD_OK).click();

			activateShellWithTitle(SHELL_TITLE);
		}

		bot.button(CMD_OK).click();
	}

}
