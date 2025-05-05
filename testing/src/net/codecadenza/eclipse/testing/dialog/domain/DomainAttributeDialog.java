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
package net.codecadenza.eclipse.testing.dialog.domain;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import net.codecadenza.eclipse.testing.domain.ElementCollectionType;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.keyboard.Keystrokes;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;

/**
 * <p>
 * Dialog for creating a domain attribute
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAttributeDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Create new domain attribute";
	private static final String LBL_ELEMENT_COLLECTION_TYPE = "Collection type:";
	private static final String LBL_ELEMENT_COLLECTION_STRATEGY = "Collection strategy:";
	private static final String LBL_PRIMARY_KEY = "Primary key:";
	private static final String LBL_JAVA_TYPE = "Java type:";
	private static final String LBL_NAME = "Name:";
	private static final String TAB_ITEM_ELEMENT_COLLECTION = "Element collection";
	private static final String ELEMENT_COLLECTION_STRATEGY_TABLE = "TABLE";

	private final DomainAttribute domainAttribute;

	/**
	 * Constructor
	 * @param bot
	 * @param domainAttribute
	 */
	public DomainAttributeDialog(SWTWorkbenchBot bot, DomainAttribute domainAttribute) {
		super(bot, SHELL_TITLE);

		this.domainAttribute = domainAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final SWTBotText txtName = bot.textWithLabel(LBL_NAME);
		txtName.setText(domainAttribute.getName());
		txtName.pressShortcut(Keystrokes.TAB);

		selectProposalItem(LBL_JAVA_TYPE, domainAttribute.getType());

		if (domainAttribute.isPrimaryKey())
			bot.checkBoxWithLabel(LBL_PRIMARY_KEY).select();

		if (domainAttribute.getElementCollectionType() != ElementCollectionType.NONE) {
			bot.tabItem(TAB_ITEM_ELEMENT_COLLECTION).activate();
			bot.comboBoxWithLabel(LBL_ELEMENT_COLLECTION_TYPE).setSelection(domainAttribute.getElementCollectionType().name());
			bot.comboBoxWithLabel(LBL_ELEMENT_COLLECTION_STRATEGY).setSelection(ELEMENT_COLLECTION_STRATEGY_TABLE);
		}

		bot.button(CMD_OK).click();
	}

}
