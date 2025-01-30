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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.testing.dialog.DeleteDialog;
import net.codecadenza.eclipse.testing.dialog.dto.EditDataTransferObjectDialog;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing data transfer objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataTransferObjectBot extends AbstractBot {
	private static final String DTO_SUFFIX = "DTO";
	private static final Logger log = LoggerFactory.getLogger(DataTransferObjectBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public DataTransferObjectBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Open one data transfer object for every domain object of the given project
	 * @param project
	 */
	public void openDTOs(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			final var domainObjectName = domainObject.getName();

			log.debug("Open data transfer object for domain object '{}'", domainObjectName);

			projectView.openDTOInEditor(domainObjectName + DTO_SUFFIX, domainObject.getNamespace());
		}

		bot.closeAllEditors();
	}

	/**
	 * Edit one data transfer object for every domain object of the given project
	 * @param project
	 */
	public void editDTOs(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			final var domainObjectName = domainObject.getName();

			log.debug("Edit data transfer object for domain object '{}'", domainObjectName);

			projectView.clickContextMenuEditDTO(domainObjectName + DTO_SUFFIX, domainObject.getNamespace());
			new EditDataTransferObjectDialog(bot).enterData();
		}
	}

	/**
	 * Delete all data transfer object of the given project
	 * @param project
	 */
	public void deleteDTOs(Project project) {
		final var projectView = new ProjectView(bot, project);
		final var dtosToDelete = projectView.getDTOTreeNodes();
		final var domainObjects = project.getNonAbstractDomainObjects();

		Collections.reverse(domainObjects);

		// Delete the data transfer objects in the correct order
		for (final var domainObject : domainObjects) {
			final List<String> domainObjectDTOs = new ArrayList<>();

			for (final var dtoName : dtosToDelete)
				if (dtoName.startsWith(domainObject.getName()))
					domainObjectDTOs.add(dtoName);

			for (final var dtoName : domainObjectDTOs) {
				log.debug("Delete data transfer object '{}'", dtoName);

				projectView.clickContextMenuDeleteDTO(dtoName, domainObject.getNamespace());
				new DeleteDialog(bot).enterData();
			}
		}
	}

}
