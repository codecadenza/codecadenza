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
package net.codecadenza.eclipse.ui.view.menu;

import java.util.Set;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.emf.common.util.Monitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent repository namespaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryNamespaceMenuBuilder extends AbstractMenuBuilder<Namespace> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public RepositoryNamespaceMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuRepository = new Menu(tree);

		// Add a menu item to synchronize the repositories
		final var itemSync = new MenuItem(menuRepository, SWT.NONE);
		itemSync.setText(TITLE_SYNC);

		itemSync.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Namespace ns = getSelectedObject();
				final Project project = ns.getProject();

				try {
					view.getViewSite().getWorkbenchWindow().run(true, false, monitor -> {
						// Begin new task
						monitor.beginTask("Synchronize...", Monitor.UNKNOWN);

						try {
							final var repositoryService = new RepositoryService(project);
							final Set<Repository> repositories = repositoryService.synchRepositories();

							EclipseIDEService.saveProjectMetaData(project);

							// Rebuild new or changed repository source files
							for (final Repository repository : repositories)
								repositoryService.rebuildRepositorySourceFiles(repository);
						}
						catch (final Exception ex) {
							shell.getDisplay().syncExec(() -> CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex));
						}

						// Refresh the tree view
						shell.getDisplay().syncExec(view::refreshTree);

						monitor.done();
					});
				}
				catch (final InterruptedException ex) {
					Thread.currentThread().interrupt();
					CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
				}
			}
		});

		return menuRepository;
	}

}
