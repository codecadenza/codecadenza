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
package net.codecadenza.eclipse.testing.editor;

import static org.junit.jupiter.api.Assertions.assertTrue;

import net.codecadenza.eclipse.testing.bots.AbstractBot;
import net.codecadenza.eclipse.testing.dialog.reverse.EditDomainAttributeDialog;
import net.codecadenza.eclipse.testing.dialog.reverse.NewEnumDialog;
import net.codecadenza.eclipse.testing.domain.AssociationType;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import net.codecadenza.eclipse.testing.domain.DomainAssociation;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import net.codecadenza.eclipse.testing.domain.Project;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Editor for performing reverse engineering
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEngineeringEditor extends AbstractBot {
	private static final String DIALOG_TITLE_EDITOR = "Reverse engineering editor";
	private static final String DIALOG_TITLE_CONFIGURATION = "Edit reverse engineering configuration";
	private static final String MNU_ADD_ENUM = "Add enum";
	private static final String MNU_ADD_ONE_TO_MANY = "Add one-to-many association";
	private static final String MNU_CONVERT_TO_ONE_TO_ONE = "Convert to one-to-one association";
	private static final String TOOL_ITEM_EDIT_CONFIGURATION = "Edit configuration";
	private static final String TOOL_ITEM_SAVE = "Save domain model";
	private static final Logger log = LoggerFactory.getLogger(ReverseEngineeringEditor.class);

	private final SWTBotEditor editor;
	private final Project project;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 */
	public ReverseEngineeringEditor(SWTWorkbenchBot bot, Project project) {
		super(bot);

		this.editor = bot.activeEditor();
		this.project = project;
	}

	/**
	 * Open the editor and save the domain model
	 */
	public void open() {
		log.debug("Waiting for initialization");
		waitForPendingBackgroundJobs();

		assertTrue(bot.table().rowCount() > 0, "No objects found!");

		openConfigurationDialog();
		changeInitialDomainModel();
		openDialogs();

		bot.toolbarButtonWithTooltip(TOOL_ITEM_SAVE).click();

		waitForSuccessDialog();

		log.debug("Reverse engineering finished successfully");

		editor.close();
	}

	/**
	 * Open the respective dialogs for all domain objects and their associations
	 */
	private void openDialogs() {
		for (final var domainObject : project.getDomainObjects()) {
			openDomainObjectDialog(domainObject.getName());

			for (final var assoc : domainObject.getAssociations())
				openDomainAssociationDialog(domainObject.getName(), assoc);
		}
	}

	/**
	 * Open the dialog for the given domain object
	 * @param domainObjectName
	 */
	private void openDomainObjectDialog(String domainObjectName) {
		final var rootTreeItem = getRootTreeItem();

		log.debug("Open dialog for domain object '{}'", domainObjectName);
		rootTreeItem.getNode(domainObjectName).contextMenu(MNU_EDIT).click();
		bot.button(CMD_OK).click();
	}

	/**
	 * Open the dialog for the given domain association
	 * @param domainObjectName
	 * @param assoc
	 */
	private void openDomainAssociationDialog(String domainObjectName, DomainAssociation assoc) {
		final var rootTreeItem = getRootTreeItem();

		for (final String node : rootTreeItem.getNode(domainObjectName).getNodes()) {
			final String assocName = assoc.toString();

			if (node.startsWith(assocName)) {
				log.debug("Open dialog for domain association '{}'", assocName);

				rootTreeItem.getNode(domainObjectName).getNode(node).contextMenu(MNU_EDIT).click();
				bot.button(CMD_OK).click();
			}
		}
	}

	/**
	 * Change the initial domain model
	 */
	private void changeInitialDomainModel() {
		createEnums();
		editDomainAttributes();
		initOneToManyAssociations();
		initOneToOneAssociations();
	}

	/**
	 * Create all enumerations
	 */
	private void createEnums() {
		final var rootTreeItem = getRootTreeItem();

		// Add all necessary enumerations
		for (final var enumeration : project.getEnumerations()) {
			rootTreeItem.contextMenu(MNU_ADD_ENUM).click();
			new NewEnumDialog(bot, enumeration).enterData();
		}
	}

	/**
	 * Edit domain attributes
	 */
	private void editDomainAttributes() {
		// Change the type of respective fields to be in sync with the domain model
		for (final var domainObject : project.getDomainObjects()) {
			for (final var domainAttribute : domainObject.getAttributes()) {
				final var rootTreeItem = getRootTreeItem();

				for (final var node : rootTreeItem.getNodes())
					if (node.equals(domainObject.getName())) {
						String existingAttribute = null;

						if (domainAttribute.getType().equals(DomainAttribute.TYPE_UUID))
							existingAttribute = DomainAttribute.TYPE_STRING + " " + domainAttribute.getName();
						else if (domainAttribute.getType().equals(DomainAttribute.TYPE_BIG_DECIMAL)
								&& project.getDataSource().getDatabaseVendor() == DatabaseVendor.POSTGRESQL)
							existingAttribute = DomainAttribute.TYPE_DOUBLE + " " + domainAttribute.getName();
						else if (domainAttribute.getType().equals(DomainAttribute.TYPE_CALENDAR)
								|| domainAttribute.getType().equals(DomainAttribute.TYPE_DATE))
							existingAttribute = DomainAttribute.TYPE_LOCAL_DATE + " " + domainAttribute.getName();

						if (existingAttribute != null) {
							log.debug("Edit domain attribute '{}'", domainAttribute.getName());
							rootTreeItem.getNode(node).getNode(existingAttribute).contextMenu(MNU_EDIT).click();
							new EditDomainAttributeDialog(bot, domainAttribute.getType()).enterData();
						}

						break;
					}
			}
		}

		// Change the domain attribute type for all enumerations
		for (final var domainObject : project.getDomainObjects())
			for (final var enumAssoc : domainObject.getEnumAssociations()) {
				final var rootTreeItem = getRootTreeItem();

				for (final var node : rootTreeItem.getNodes())
					if (node.equals(domainObject.getName())) {
						final var existingAttribute = DomainAttribute.TYPE_STRING + " " + enumAssoc.getName();

						log.debug("Edit domain attribute '{}'", enumAssoc.getName());
						rootTreeItem.getNode(node).getNode(existingAttribute).contextMenu(MNU_EDIT).click();
						new EditDomainAttributeDialog(bot, enumAssoc.getTarget().getName()).enterData();
						break;
					}
			}
	}

	/**
	 * Create all one-to-many associations
	 */
	private void initOneToManyAssociations() {
		final var rootTreeItem = getRootTreeItem();

		for (final var domainObject : project.getDomainObjects()) {
			for (final var assoc : domainObject.getAssociations()) {
				if (assoc.getType() != AssociationType.ONE_TO_MANY)
					continue;

				log.debug("Initialize one-to-many association '{}'", assoc.getName());

				final var manyToOneNode = domainObject.getName() + " " + domainObject.getName().toLowerCase();
				rootTreeItem.getNode(assoc.getTarget().getName()).getNode(manyToOneNode).contextMenu(MNU_ADD_ONE_TO_MANY).click();
			}
		}
	}

	/**
	 * Create all one-to-one associations
	 */
	private void initOneToOneAssociations() {
		final var rootTreeItem = getRootTreeItem();

		for (final var domainObject : project.getDomainObjects()) {
			for (final var assoc : domainObject.getAssociations()) {
				if (assoc.getType() != AssociationType.ONE_TO_ONE)
					continue;

				log.debug("Initialize one-to-one association '{}'", assoc.getName());

				final var manyToOneNode = assoc.getTarget().getName() + " " + assoc.getTarget().getName().toLowerCase();
				rootTreeItem.getNode(domainObject.getName()).getNode(manyToOneNode).contextMenu(MNU_CONVERT_TO_ONE_TO_ONE).click();
			}
		}
	}

	/**
	 * Expand the tree and get the root tree item
	 * @return the root tree item
	 */
	private SWTBotTreeItem getRootTreeItem() {
		final var rootTreeItem = editor.bot().tree().getTreeItem(Project.DEFAULT_NAMESPACE);

		// Expand all domain object tree items
		for (final var node : rootTreeItem.getNodes())
			rootTreeItem.getNode(node).expand();

		return rootTreeItem;
	}

	/**
	 * Wait for the dialog that indicates that the domain model has been saved successfully
	 */
	private void waitForSuccessDialog() {
		bot.shell(DIALOG_TITLE_EDITOR).activate();
		bot.button().click();
	}

	/**
	 * Open the configuration dialog
	 */
	private void openConfigurationDialog() {
		bot.toolbarButtonWithTooltip(TOOL_ITEM_EDIT_CONFIGURATION).click();
		bot.shell(DIALOG_TITLE_CONFIGURATION).activate();
		bot.button(CMD_OK).click();
		bot.shell(DIALOG_TITLE_EDITOR).activate();
		bot.button(CMD_OK).click();

		waitForPendingBackgroundJobs();
	}

}
