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
package net.codecadenza.eclipse.testing.view;

import java.time.Duration;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.testing.domain.Project;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class for checking if compilation errors in the 'Problems' view exist
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProblemsView extends AbstractView {
	private static final Duration MAX_DURATION_FOR_NO_ERRORS = Duration.ofMinutes(2);
	private static final int MAX_NUMBER_CHECKS = 10;
	private static final String MOJO_ERROR = "Failed to execute mojo";
	private static final String TREE_ITEM_ERRORS = "Errors";
	private static final String UNKNOWN_ERROR = "Unknown";
	private static final String VIEW_TITLE = "Problems";
	private static final Logger log = LoggerFactory.getLogger(ProblemsView.class);

	private final Project project;
	private Set<String> errorMessages = new HashSet<>();
	private int unknownErrorCount;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 */
	public ProblemsView(SWTWorkbenchBot bot, Project project) {
		super(bot, VIEW_TITLE);

		this.project = project;
	}

	/**
	 * Open the 'Problems' view and wait until all compilation errors are gone
	 */
	public void waitForNoErrors() {
		final var packageExplorer = new PackageExplorerView(bot, project);

		bot.waitUntil(new ICondition() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swtbot.swt.finder.waits.ICondition#test()
			 */
			@Override
			public boolean test() throws Exception {
				// Solve "temporary" problems by refreshing all projects
				packageExplorer.refreshProjects();

				final var problemsView = showView();
				errorMessages = new HashSet<>();

				// The error markers must be checked multiple times to be sure that there are really no errors!
				for (int i = 0; i < MAX_NUMBER_CHECKS; i++) {
					final var treeItems = problemsView.bot().tree().getAllItems();

					for (final var item : treeItems) {
						if (!item.isExpanded())
							item.expand();

						if (hasErrors(item)) {
							// Ignore 'Unknown' errors that cannot be solved even after performing multiple checks
							if (unknownErrorCount == MAX_NUMBER_CHECKS) {
								log.info("Ignoring 'Unknown' error(s)");
								return true;
							}

							return false;
						}
					}

					bot.sleep(300);
				}

				log.info("Project '{}' contains no errors!", project.getName());
				return true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swtbot.swt.finder.waits.ICondition#init(org.eclipse.swtbot.swt.finder.SWTBot)
			 */
			@Override
			public void init(SWTBot bot) {
				unknownErrorCount = 0;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swtbot.swt.finder.waits.ICondition#getFailureMessage()
			 */
			@Override
			public String getFailureMessage() {
				log.error("Project '{}' could not be built successfully due to the following error(s):", project.getName());

				for (final String errorMessage : errorMessages)
					log.error("{}", errorMessage);

				return "Project contains compilation errors!";
			}
		}, MAX_DURATION_FOR_NO_ERRORS.toMillis());
	}

	private boolean hasErrors(SWTBotTreeItem treeItem) {
		final var errorItems = treeItem.getItems();

		if (!treeItem.getText().startsWith(TREE_ITEM_ERRORS) || errorItems.length == 0)
			return false;

		log.warn("Project '{}' contains {} error(s)", project.getName(), errorItems.length);

		for (final var errorItem : errorItems)
			errorMessages.add(errorItem.cell(0));

		// Ignore occasionally occurring Maven mojo execution errors
		errorMessages = errorMessages.stream().filter(message -> !message.startsWith(MOJO_ERROR)).collect(Collectors.toSet());

		if (errorMessages.isEmpty())
			return false;

		if (errorMessages.contains(UNKNOWN_ERROR) && errorMessages.size() == 1) {
			unknownErrorCount++;

			log.trace("Number of checks with 'Unknown' errors: {}", unknownErrorCount);
		}

		return true;
	}

}
