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
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

/**
 * <p>
 * Dialog for creating a data exchange object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateExchangeMethodDialog extends AbstractDialog {
	private static final String INVOCATION_MODE_ASYNC = "ASYNCHRONOUS";
	private static final String INVOCATION_MODE_SCHEDULED = "SCHEDULED";
	private static final String LBL_CONTENT_TYPE = "Content type:";
	private static final String LBL_DOMAIN_OBJECT = "Select domain object:";
	private static final String LBL_DOMAIN_OBJECT_TREE_VIEW = "Drag and drop items to be exchanged:";
	private static final String LBL_INVOCATION_MODE = "Select mode:";
	private static final String LBL_MAPPING_STRUCTURE = "Mapping (file) structure:";
	private static final String LBL_METHOD_TYPE = "Method type:";
	private static final String LBL_PROCESS_SINGLE_OBJECT = "Process single object:";
	private static final String METHOD_TYPE_EXPORT = "EXPORT";
	private static final String MNU_CREATE_NEW_PARAMETER = "Create new parameter";
	private static final String TAB_ITEM_FILTER_PARAMETERS = "Filter parameters";
	private static final String TAB_ITEM_INVOCATION_SETTINGS = "Invocation settings";
	private static final String SHELL_TITLE = "Create new data exchange method";

	private final Project project;
	private final DataExchangeMethod exchangeMethod;
	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 * @param exchangeMethod
	 */
	public CreateExchangeMethodDialog(SWTWorkbenchBot bot, Project project, DataExchangeMethod exchangeMethod) {
		super(bot, SHELL_TITLE);

		this.project = project;
		this.exchangeMethod = exchangeMethod;
		this.domainObject = exchangeMethod.getDomainObject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		selectProposalItem(LBL_DOMAIN_OBJECT, domainObject.getName());

		if (exchangeMethod.isProcessSingleObject())
			bot.checkBoxWithLabel(LBL_PROCESS_SINGLE_OBJECT).click();

		if (!exchangeMethod.isImportData())
			bot.comboBoxWithLabel(LBL_METHOD_TYPE).setSelection(METHOD_TYPE_EXPORT);

		if (exchangeMethod.getContentType() != ContentType.XML)
			bot.comboBoxWithLabel(LBL_CONTENT_TYPE).setSelection(exchangeMethod.getContentType().name());

		if (project.getTechnologyPlatform() != TechnologyPlatform.JAVA_SE) {
			bot.tabItem(TAB_ITEM_INVOCATION_SETTINGS).activate();

			if (project.getTechnologyPlatform() == TechnologyPlatform.SPRING_BOOT)
				bot.comboBoxWithLabel(LBL_INVOCATION_MODE).setSelection(INVOCATION_MODE_SCHEDULED);
			else if (exchangeMethod.isImportData())
				bot.comboBoxWithLabel(LBL_INVOCATION_MODE).setSelection(INVOCATION_MODE_ASYNC);
		}

		final var domainObjectTreeView = bot.treeWithLabel(LBL_DOMAIN_OBJECT_TREE_VIEW);
		final var treeViewStructure = bot.treeWithLabel(LBL_MAPPING_STRUCTURE);
		final var domainObjectTreeItem = domainObjectTreeView.getTreeItem(domainObject.getName());
		SWTBotTreeItem elementsTreeItem = null;
		SWTBotTreeItem attributesTreeItem = null;

		if (exchangeMethod.getContentType() == ContentType.XML) {
			if (exchangeMethod.isProcessSingleObject()) {
				elementsTreeItem = treeViewStructure.getAllItems()[0].getItems()[0];
				attributesTreeItem = treeViewStructure.getAllItems()[0].getItems()[1];
			}
			else {
				elementsTreeItem = treeViewStructure.getAllItems()[0].getItems()[0].getItems()[0];
				attributesTreeItem = treeViewStructure.getAllItems()[0].getItems()[0].getItems()[1];
			}
		}
		else if (exchangeMethod.getContentType() == ContentType.JSON) {
			elementsTreeItem = treeViewStructure.getAllItems()[0].getItems()[0];
			attributesTreeItem = treeViewStructure.getAllItems()[0].getItems()[1];
		}
		else
			attributesTreeItem = treeViewStructure.getAllItems()[0].getItems()[0];

		for (final var domainAttribute : domainObject.getAllAttributes()) {
			final var attrTreeItem = domainObjectTreeItem.getNode(domainAttribute.toString());

			attrTreeItem.dragAndDrop(attributesTreeItem);
		}

		for (final var domainAssociation : domainObject.getAssociations()) {
			final var targetDomainObject = domainAssociation.getTarget();
			final var assocRootTreeItemText = domainAssociation.toString();
			final var assocRootTreeItem = domainObjectTreeView.getTreeItem(domainObject.getName()).getNode(assocRootTreeItemText)
					.select().expand();

			if (!domainAssociation.isToManyAssociation()) {
				// Add all attributes of the target domain object to the mapping structure
				for (final var targetAttribute : targetDomainObject.getAllAttributes()) {
					final var attrTreeItem = assocRootTreeItem.getNode(targetAttribute.toString());

					attrTreeItem.dragAndDrop(attributesTreeItem);
				}
			}
			else if (elementsTreeItem != null) {
				assocRootTreeItem.dragAndDrop(elementsTreeItem);

				final var subItemAttributesTreeItem = elementsTreeItem.getItems()[0].getItems()[1];

				for (final var targetAttribute : targetDomainObject.getAllAttributes()) {
					final var attrTreeItem = assocRootTreeItem.getNode(targetAttribute.toString());

					attrTreeItem.dragAndDrop(subItemAttributesTreeItem);
				}
			}
		}

		if (!exchangeMethod.isProcessSingleObject() && !exchangeMethod.isImportData()) {
			// Add a filter parameter to the data export method
			bot.tabItem(TAB_ITEM_FILTER_PARAMETERS).activate();

			bot.table().contextMenu(MNU_CREATE_NEW_PARAMETER).click();
			new CreateFilterParameterDialog(bot).enterData();
			activateShellWithTitle(SHELL_TITLE);
		}

		bot.button(CMD_OK).click();
	}

}
