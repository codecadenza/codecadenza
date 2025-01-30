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
import net.codecadenza.eclipse.testing.domain.DomainObject;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating an integration bean
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateIntegrationBeanDialog extends AbstractDialog {
	private static final String LBL_DOMAIN_OBJECT = "Domain object:";
	private static final String LBL_AVAILABLE_METHODS_TREE_VIEW = "Available methods";
	private static final String LBL_INTEGRATION_METHODS_TREE_VIEW = "Integration methods";
	private static final String SHELL_TITLE = "Create new integration bean";

	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 */
	public CreateIntegrationBeanDialog(SWTWorkbenchBot bot, DomainObject domainObject) {
		super(bot, SHELL_TITLE);

		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		selectProposalItem(LBL_DOMAIN_OBJECT, domainObject.getName());

		final var availableMethodsTreeView = bot.treeWithLabel(LBL_AVAILABLE_METHODS_TREE_VIEW);
		final var integrationMethodsTreeView = bot.treeWithLabel(LBL_INTEGRATION_METHODS_TREE_VIEW);
		final var boundaryMethods = new ArrayList<String>();

		for (final var boundaryMethodTreeItem : availableMethodsTreeView.getAllItems()[0].getItems())
			boundaryMethods.add(boundaryMethodTreeItem.getText());

		// Create an integration method for every boundary method
		for (final var boundaryMethodName : boundaryMethods) {
			final var boundaryMethodsTreeItem = availableMethodsTreeView.getAllItems()[0];
			final var boundaryMethodTreeItem = boundaryMethodsTreeItem.getNode(boundaryMethodName);

			boundaryMethodTreeItem.dragAndDrop(integrationMethodsTreeView.getAllItems()[0]);
		}

		bot.button(CMD_OK).click();
	}

}
