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
package net.codecadenza.eclipse.testing.dialog.integration;

import java.util.ArrayList;
import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.IntegrationBeanType;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for editing an integration bean
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditIntegrationBeanDialog extends AbstractDialog {
	private static final String LBL_INTEGRATION_METHODS_TREE_VIEW = "Integration methods";
	private static final String SHELL_TITLE = "Edit integration bean";
	private static final Logger log = LoggerFactory.getLogger(EditIntegrationBeanDialog.class);

	private final IntegrationBeanType integrationBeanType;

	/**
	 * Constructor
	 * @param bot
	 * @param integrationBeanType
	 */
	public EditIntegrationBeanDialog(SWTWorkbenchBot bot, IntegrationBeanType integrationBeanType) {
		super(bot, SHELL_TITLE);

		this.integrationBeanType = integrationBeanType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var integrationMethodsTreeView = bot.treeWithLabel(LBL_INTEGRATION_METHODS_TREE_VIEW);
		final var integrationMethods = new ArrayList<String>();

		for (final var integrationMethodTreeItem : integrationMethodsTreeView.getAllItems()[0].getItems())
			integrationMethods.add(integrationMethodTreeItem.getText());

		for (final var integrationMethod : integrationMethods) {
			log.debug("Edit integration method '{}'", integrationMethod);

			integrationMethodsTreeView.getAllItems()[0].getNode(integrationMethod).doubleClick();
			new EditIntegrationMethodDialog(bot, integrationBeanType).enterData();
			activateShellWithTitle(SHELL_TITLE);
		}

		bot.button(CMD_OK).click();
	}

}
