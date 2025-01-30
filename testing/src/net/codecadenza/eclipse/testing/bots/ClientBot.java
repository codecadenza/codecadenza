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
package net.codecadenza.eclipse.testing.bots;

import java.util.Collections;
import net.codecadenza.eclipse.testing.dialog.DeleteDialog;
import net.codecadenza.eclipse.testing.dialog.RenameCompilationUnitDialog;
import net.codecadenza.eclipse.testing.dialog.RenameDialog;
import net.codecadenza.eclipse.testing.dialog.AbstractDialog.OperationMode;
import net.codecadenza.eclipse.testing.dialog.client.CreateDefaultFormsDialog;
import net.codecadenza.eclipse.testing.dialog.client.CreateGridPanelDialog;
import net.codecadenza.eclipse.testing.dialog.client.CreateSingleRecordFormDialog;
import net.codecadenza.eclipse.testing.dialog.client.CreateTreeViewDialog;
import net.codecadenza.eclipse.testing.dialog.client.CreateViewFormDialog;
import net.codecadenza.eclipse.testing.dialog.client.EditGridPanelDialog;
import net.codecadenza.eclipse.testing.dialog.client.EditSingleRecordFormDialog;
import net.codecadenza.eclipse.testing.dialog.client.EditTreeViewDialog;
import net.codecadenza.eclipse.testing.dialog.client.EditViewFormDialog;
import net.codecadenza.eclipse.testing.domain.DomainAssociation;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.FormType;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing client dialogs and views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ClientBot extends AbstractBot {
	private static final String GRID_PANEL_SUFFIX = "Panel";
	private static final Logger log = LoggerFactory.getLogger(ClientBot.class);

	private final OperationMode operationMode;

	/**
	 * Constructor
	 * @param bot
	 * @param operationMode
	 */
	public ClientBot(SWTWorkbenchBot bot, OperationMode operationMode) {
		super(bot);

		this.operationMode = operationMode;
	}

	/**
	 * Constructor
	 * @param bot
	 */
	public ClientBot(SWTWorkbenchBot bot) {
		this(bot, OperationMode.NOT_SET);
	}

	/**
	 * Create dialogs and views for the existing domain objects
	 * @param project
	 */
	public void createForms(Project project) {
		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			createForms(project, domainObject);
			createTreeView(project, domainObject);
			waitForPendingBackgroundJobs();
		}

		bot.closeAllEditors();
	}

	/**
	 * Edit all dialogs, grid panels and views of the given project
	 * @param project
	 */
	public void editForms(Project project) {
		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			editForms(project, domainObject);
			waitForPendingBackgroundJobs();
		}
	}

	/**
	 * Rename all dialogs, grid panels and views of the given project
	 * @param project
	 */
	public void renameForms(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final String formName : projectView.getFormNames()) {
			final String newName = formName + NEW_NAME_SUFFIX;

			log.debug("Rename form '{}'", formName);

			projectView.clickContextMenuRenameForm(formName);
			new RenameDialog(bot, newName).enterData();
			new RenameCompilationUnitDialog(bot).enterData();

			projectView.openFormInEditor(newName);
		}
	}

	/**
	 * Delete all dialogs, grid panels and views of the given project
	 * @param project
	 */
	public void deleteForms(Project project) {
		final var domainObjects = project.getNonAbstractDomainObjects();
		Collections.reverse(domainObjects);

		for (final var domainObject : domainObjects) {
			log.debug("Delete forms of domain object '{}'", domainObject.getName());

			deleteForms(project, domainObject);
		}
	}

	private void createForms(Project project, DomainObject domainObject) {
		final var projectView = new ProjectView(bot, project);
		final var domainObjectName = domainObject.getName();

		if (domainObject.isCreateDefaultForms()) {
			log.debug("Create default forms for domain object '{}'", domainObjectName);

			projectView.clickContextMenuDefaultForms();
			new CreateDefaultFormsDialog(bot, domainObjectName).enterData();

			// Open all generated forms
			projectView.openFormInEditor(domainObject.getCreateFormName());
			projectView.openFormInEditor(domainObject.getEditFormName());
			projectView.openFormInEditor(domainObject.getReadonlyFormName());
			projectView.openFormInEditor(domainObject.getViewFormName());
		}
		else {
			log.debug("Create forms for domain object '{}'", domainObjectName);

			for (final var association : domainObject.getAssociations()) {
				if (association.isToManyAssociation()) {
					projectView.clickContextMenuCreateGridPanel();
					new CreateGridPanelDialog(bot, domainObjectName, association).enterData();
				}
			}

			projectView.clickContextMenuCreateUpdateForm();
			new CreateSingleRecordFormDialog(bot, domainObject, FormType.CREATE).enterData();
			projectView.openFormInEditor(domainObject.getCreateFormName());

			projectView.clickContextMenuCreateUpdateForm();
			new CreateSingleRecordFormDialog(bot, domainObject, FormType.UPDATE).enterData();
			projectView.openFormInEditor(domainObject.getEditFormName());

			projectView.clickContextMenuCreateUpdateForm();
			new CreateSingleRecordFormDialog(bot, domainObject, FormType.READONLY).enterData();
			projectView.openFormInEditor(domainObject.getReadonlyFormName());

			projectView.clickContextMenuCreateViewForm();
			new CreateViewFormDialog(bot, domainObject, FormType.SEARCHABLE_VIEW).enterData();
			projectView.openFormInEditor(domainObject.getViewFormName());

			projectView.clickContextMenuCreateViewForm();
			new CreateViewFormDialog(bot, domainObject, FormType.LOV).enterData();
			projectView.openFormInEditor(domainObject.getListOfValuesName());
		}
	}

	private void createTreeView(Project project, DomainObject domainObject) {
		log.debug("Create tree view for domain object '{}'", domainObject.getName());

		final var projectView = new ProjectView(bot, project);
		projectView.clickContextMenuCreateTreeView();
		new CreateTreeViewDialog(bot, domainObject).enterData();

		projectView.openFormInEditor(domainObject.getTreeViewName());
	}

	private void editForms(Project project, DomainObject domainObject) {
		log.debug("Edit forms for domain object '{}'", domainObject.getName());

		final var projectView = new ProjectView(bot, project);
		projectView.clickContextMenuEditForm(domainObject.getCreateFormName());
		new EditSingleRecordFormDialog(bot).enterData();

		projectView.clickContextMenuEditForm(domainObject.getEditFormName());
		new EditSingleRecordFormDialog(bot, operationMode).enterData();

		projectView.clickContextMenuEditForm(domainObject.getReadonlyFormName());
		new EditSingleRecordFormDialog(bot, operationMode).enterData();

		projectView.clickContextMenuEditForm(domainObject.getViewFormName());
		new EditViewFormDialog(bot, domainObject, operationMode).enterData();

		projectView.clickContextMenuEditForm(domainObject.getTreeViewName());
		new EditTreeViewDialog(bot).enterData();

		if (!domainObject.isCreateDefaultForms()) {
			projectView.clickContextMenuEditForm(domainObject.getListOfValuesName());
			new EditViewFormDialog(bot).enterData();
		}

		for (final var association : domainObject.getAssociations()) {
			if (association.isToManyAssociation()) {
				projectView.clickContextMenuEditForm(getGridPanelName(domainObject, association));
				new EditGridPanelDialog(bot).enterData();
			}
		}
	}

	private void deleteForms(Project project, DomainObject domainObject) {
		final var projectView = new ProjectView(bot, project);
		projectView.clickContextMenuDeleteForm(domainObject.getViewFormName());
		new DeleteDialog(bot).enterData();

		projectView.clickContextMenuDeleteForm(domainObject.getTreeViewName());
		new DeleteDialog(bot).enterData();

		projectView.clickContextMenuDeleteForm(domainObject.getCreateFormName());
		new DeleteDialog(bot).enterData();

		projectView.clickContextMenuDeleteForm(domainObject.getEditFormName());
		new DeleteDialog(bot).enterData();

		projectView.clickContextMenuDeleteForm(domainObject.getReadonlyFormName());
		new DeleteDialog(bot).enterData();

		if (!domainObject.isCreateDefaultForms()) {
			projectView.clickContextMenuDeleteForm(domainObject.getListOfValuesName());
			new DeleteDialog(bot).enterData();
		}

		for (final var association : domainObject.getAssociations()) {
			if (association.isToManyAssociation()) {
				projectView.clickContextMenuDeleteForm(getGridPanelName(domainObject, association));
				new DeleteDialog(bot).enterData();
			}
		}
	}

	private String getGridPanelName(DomainObject domainObject, DomainAssociation association) {
		final var domainObjectName = domainObject.getName();
		final var panelPrefixDomainObject = domainObjectName.substring(0, 1).toUpperCase() + domainObjectName.substring(1);
		final var assocName = association.getName().substring(0, 1).toUpperCase() + association.getName().substring(1);

		return panelPrefixDomainObject + assocName + GRID_PANEL_SUFFIX;
	}

}
