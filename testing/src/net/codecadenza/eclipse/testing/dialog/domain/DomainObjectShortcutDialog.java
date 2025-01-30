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
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.Project;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating a domain object shortcut
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectShortcutDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Select model element";
	private static final String TREE_ITEM_MODEL = "model";

	private final Project project;
	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 * @param domainObject
	 */
	public DomainObjectShortcutDialog(SWTWorkbenchBot bot, Project project, DomainObject domainObject) {
		super(bot, SHELL_TITLE);

		this.project = project;
		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var domainArtifactName = project.getName() + "-domain";
		final var namespace = domainObject.getNamespace();
		final var modelFileName = "package-" + namespace + ".xmi";
		final var modelNamespaceName = "Domain Namespace " + namespace;
		final var modelDomainObjectName = "Domain Object " + domainObject.getName();

		bot.tree().getTreeItem(domainArtifactName).expand().getNode(TREE_ITEM_MODEL).expand().getNode(modelFileName).expand()
				.getNode(modelNamespaceName).expand().getNode(modelDomainObjectName).select();
		bot.button(CMD_OK).click();
	}

}
