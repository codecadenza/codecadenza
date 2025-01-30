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

import java.util.HashSet;
import net.codecadenza.eclipse.testing.dialog.domain.CreateManyToManyAssociationDialog;
import net.codecadenza.eclipse.testing.dialog.domain.CreateManyToOneAssociationDialog;
import net.codecadenza.eclipse.testing.dialog.domain.CreateOneToManyAssociationDialog;
import net.codecadenza.eclipse.testing.dialog.domain.CreateOneToOneAssociationDialog;
import net.codecadenza.eclipse.testing.dialog.domain.DomainAttributeDialog;
import net.codecadenza.eclipse.testing.dialog.domain.DomainObjectDialog;
import net.codecadenza.eclipse.testing.dialog.domain.DomainObjectShortcutDialog;
import net.codecadenza.eclipse.testing.dialog.domain.NewEnumAssociationDialog;
import net.codecadenza.eclipse.testing.dialog.domain.NewEnumDialog;
import net.codecadenza.eclipse.testing.dialog.domain.NewEnumLiteralDialog;
import net.codecadenza.eclipse.testing.domain.AssociationType;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProjectView;
import net.codecadenza.eclipse.testing.view.PropertiesView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefEditor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing domain objects, attributes, associations and enumerations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainBot extends AbstractBot {

	private static final int DEFAULT_WIDTH = 200;
	private static final int DEFAULT_HEIGHT = 100;
	private static final String MNU_CREATE_SHORTCUT = "Create Shortcut...";
	private static final String MNU_DELETE_FROM_MODEL = "Delete from Model";
	private static final String TOOL_DOMAIN_ATTRIBUTE = "Domain attribute";
	private static final String TOOL_ASSOCIATION_MTM = "Many-to-many association";
	private static final String TOOL_ASSOCIATION_MTO = "Many-to-one association";
	private static final String TOOL_ASSOCIATION_OTM = "One-to-many association";
	private static final String TOOL_ASSOCIATION_OTO = "One-to-one association";
	private static final String TOOL_DOMAIN_OBJECT = "Domain object";
	private static final String TOOL_ENUMERATION = "Enumeration";
	private static final String TOOL_ENUMERATION_ASSOC = "Enum association";
	private static final String TOOL_ENUMERATION_LITERAL = "Enumeration literal";
	private static final String TOOL_INHERITANCE = "Inheritance";
	private static final Logger log = LoggerFactory.getLogger(DomainBot.class);

	private final SWTGefBot gefBot;

	/**
	 * Constructor
	 * @param bot
	 */
	public DomainBot(SWTGefBot bot) {
		super(bot);

		this.gefBot = bot;
	}

	/**
	 * Draw the domain objects in the respective diagrams
	 * @param project
	 */
	public void drawDomainModel(Project project) {
		drawDomainModel(project, Project.DEFAULT_NAMESPACE);

		if (project.hasAdditionalNamespace()) {
			new ProjectBot(bot).createAdditionalNamespace(project);

			drawDomainModel(project, Project.ADDITIONAL_NAMESPACE);
		}
	}

	/**
	 * Draw the domain objects in the respective diagram of the given namespace
	 * @param project
	 * @param namespace
	 */
	private void drawDomainModel(Project project, String namespace) {
		new PropertiesView(bot).show();

		final var diagramEditorName = project.getDomainDiagramFileName(namespace);
		final SWTBotGefEditor editor = gefBot.gefEditor(diagramEditorName);
		final var shortcuts = new HashSet<DomainObject>();

		// Search for all domain objects that must be linked from other namespaces
		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace)) {
			if (domainObject.getParentDomainObject() != null && !domainObject.getParentDomainObject().getNamespace().equals(namespace))
				shortcuts.add(domainObject.getParentDomainObject());

			for (final var domainAssociation : domainObject.getAssociations())
				if (!domainAssociation.getTarget().getNamespace().equals(namespace))
					shortcuts.add(domainAssociation.getTarget());
		}

		for (final var domainObject : shortcuts)
			createShortcut(editor, project, domainObject);

		for (final var enumeration : project.getEnumerationsOfNamespace(namespace)) {
			final int fromX = enumeration.getStartXPosition();
			final int fromY = enumeration.getStartYPosition();

			log.debug("Draw enumeration '{}'", enumeration.getName());

			editor.activateTool(TOOL_ENUMERATION);
			editor.drag(fromX, fromY, fromX + DEFAULT_WIDTH, fromY + DEFAULT_HEIGHT);

			new NewEnumDialog(bot, enumeration).enterData();

			for (final var literal : enumeration.getLiterals()) {
				editor.activateTool(TOOL_ENUMERATION_LITERAL);
				editor.drag(fromX + 5, fromY + 5, fromX + 10, fromY + 10);

				new NewEnumLiteralDialog(gefBot, literal).enterData();
			}
		}

		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace)) {
			final int fromX = domainObject.getStartXPosition();
			final int fromY = domainObject.getStartYPosition();

			log.debug("Draw domain object '{}'", domainObject.getName());

			editor.activateTool(TOOL_DOMAIN_OBJECT);
			editor.drag(fromX, fromY, fromX + DEFAULT_WIDTH, fromY + DEFAULT_HEIGHT);

			new DomainObjectDialog(bot, domainObject).enterData();

			for (final var domainAttribute : domainObject.getAttributes()) {
				editor.activateTool(TOOL_DOMAIN_ATTRIBUTE);
				editor.drag(fromX + 5, fromY + 5, fromX + 10, fromY + 10);

				new DomainAttributeDialog(gefBot, domainAttribute).enterData();
			}
		}

		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace)) {
			final int fromX = domainObject.getStartXPosition();
			final int fromY = domainObject.getStartYPosition();

			for (final var domainAssociation : domainObject.getAssociations()) {
				final int toX = domainAssociation.getTarget().getStartXPosition();
				final int toY = domainAssociation.getTarget().getStartYPosition();

				log.debug("Draw association '{}'", domainAssociation.getName());

				if (domainAssociation.getType() == AssociationType.MANY_TO_ONE)
					editor.activateTool(TOOL_ASSOCIATION_MTO);
				else if (domainAssociation.getType() == AssociationType.MANY_TO_MANY)
					editor.activateTool(TOOL_ASSOCIATION_MTM);
				else if (domainAssociation.getType() == AssociationType.ONE_TO_ONE)
					editor.activateTool(TOOL_ASSOCIATION_OTO);
				else if (domainAssociation.getType() == AssociationType.ONE_TO_MANY)
					editor.activateTool(TOOL_ASSOCIATION_OTM);

				editor.drag(fromX + 5, fromY + 5, toX + 5, toY + 5);

				if (domainAssociation.getType() == AssociationType.MANY_TO_ONE)
					new CreateManyToOneAssociationDialog(bot).enterData();
				else if (domainAssociation.getType() == AssociationType.MANY_TO_MANY)
					new CreateManyToManyAssociationDialog(bot).enterData();
				else if (domainAssociation.getType() == AssociationType.ONE_TO_ONE)
					new CreateOneToOneAssociationDialog(bot).enterData();
				else if (domainAssociation.getType() == AssociationType.ONE_TO_MANY)
					new CreateOneToManyAssociationDialog(bot, domainAssociation.isBidirectional()).enterData();
			}

			for (final var enumAssociation : domainObject.getEnumAssociations()) {
				final int toX = enumAssociation.getTarget().getStartXPosition();
				final int toY = enumAssociation.getTarget().getStartYPosition();

				log.debug("Draw enum association '{}'", enumAssociation.getName());

				editor.activateTool(TOOL_ENUMERATION_ASSOC);
				editor.drag(fromX + 5, fromY + 5, toX + 5, toY + 5);

				new NewEnumAssociationDialog(gefBot, enumAssociation).enterData();
			}
		}

		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace)) {
			if (domainObject.getParentDomainObject() == null)
				continue;

			final int fromX = domainObject.getStartXPosition();
			final int fromY = domainObject.getStartYPosition();
			final int toX = domainObject.getParentDomainObject().getStartXPosition();
			final int toY = domainObject.getParentDomainObject().getStartYPosition();

			log.debug("Draw inheritance for '{}'", domainObject.getName());

			editor.activateTool(TOOL_INHERITANCE);
			editor.drag(fromX + 5, fromY + 5, toX + 5, toY + 5);
		}

		waitForPendingBackgroundJobs();
		editor.saveAndClose();

		// Open the source files for all domain objects and enumerations
		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace))
			new ProjectView(bot, project).openDomainObjectInEditor(domainObject.getName(), domainObject.getNamespace());

		for (final var enumeration : project.getEnumerationsOfNamespace(namespace))
			new ProjectView(bot, project).openEnumerationInEditor(enumeration.getName(), enumeration.getNamespace());

		bot.closeAllEditors();
	}

	/**
	 * Delete the domain objects
	 * @param project
	 * @param namespace
	 */
	public void deleteDomainModel(Project project, String namespace) {
		new PackageExplorerView(gefBot, project).openDomainDiagram(namespace);

		final var diagramEditorName = project.getDomainDiagramFileName(namespace);
		final SWTBotGefEditor editor = gefBot.gefEditor(diagramEditorName);

		// Delete the domain associations
		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace))
			for (final var domainAssociation : domainObject.getAssociations())
				deleteDomainAssociation(editor, domainAssociation.getName());

		// Delete all domain objects
		for (final var domainObject : project.getDomainObjectsOfNamespace(namespace)) {
			log.debug("Delete domain object '{}'", domainObject.getName());

			deleteEditPart(editor, domainObject.getName());
		}

		// Delete all enumerations
		for (final var enumeration : project.getEnumerations()) {
			log.debug("Delete enumeration '{}'", enumeration.getName());

			deleteEditPart(editor, enumeration.getName());
		}

		editor.saveAndClose();
	}

	private void createShortcut(SWTBotGefEditor editor, Project project, DomainObject domainObject) {
		log.debug("Create shortcut for domain object '{}'", domainObject.getName());

		editor.clickContextMenu(MNU_CREATE_SHORTCUT);
		new DomainObjectShortcutDialog(bot, project, domainObject).enterData();
	}

	private void deleteEditPart(SWTBotGefEditor editor, String name) {
		final var editPart = editor.getEditPart(name);

		if (editPart != null) {
			editPart.click();
			editor.clickContextMenu(MNU_DELETE_FROM_MODEL);
		}
	}

	private void deleteDomainAssociation(final SWTBotGefEditor editor, final String assocName) {
		log.debug("Delete domain association '{}'", assocName);

		editor.getEditPart(assocName).select();
		editor.clickContextMenu(MNU_DELETE_FROM_MODEL);
	}

}
