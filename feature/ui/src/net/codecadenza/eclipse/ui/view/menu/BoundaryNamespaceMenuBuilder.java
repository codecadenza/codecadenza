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

import static net.codecadenza.eclipse.shared.Constants.IMG_REFRESH;

import java.util.Set;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.boundary.EditBoundaryBeanDialog;
import net.codecadenza.eclipse.ui.dialog.boundary.NewBoundaryBeanDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.emf.common.util.Monitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent boundary namespaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryNamespaceMenuBuilder extends AbstractMenuBuilder<Namespace> {
	private final boolean boundaryMode;

	/**
	 * Constructor
	 * @param view
	 * @param tree
	 * @param boundaryMode
	 */
	public BoundaryNamespaceMenuBuilder(ProjectView view, Tree tree, boolean boundaryMode) {
		super(view, tree);

		this.boundaryMode = boundaryMode;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuBoundary = new Menu(tree);

		if (boundaryMode) {
			// Add a menu item to create a new boundary bean
			final var itemAdd = new MenuItem(menuBoundary, SWT.NONE);
			itemAdd.setText(TITLE_NEW);

			itemAdd.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (EclipseIDEService.openDiagramExists(view))
						return;

					// Open the dialog to create a new boundary bean
					final var dlgNew = new NewBoundaryBeanDialog(shell, getSelectedObject().getProject());

					if (dlgNew.open() != Dialog.OK)
						return;

					// Open the dialog to edit the newly created boundary bean
					final var dlgEdit = new EditBoundaryBeanDialog(shell, dlgNew.getBoundary());

					// The boundary must be rebuilt if the user hasn't pressed the 'OK' button!
					if (dlgEdit.open() != Dialog.OK)
						try {
							new BoundaryService(getSelectedObject().getProject()).rebuildBoundarySourceFiles(dlgNew.getBoundary());
						}
						catch (final Exception ex) {
							CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
						}

					view.refreshTree();
				}
			});
		}
		else {
			// Add a menu item to synchronize the facades
			final var itemSync = new MenuItem(menuBoundary, SWT.NONE);
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

					final Namespace namespace = getSelectedObject();
					final Project project = namespace.getProject();

					try {
						view.getViewSite().getWorkbenchWindow().run(true, false, monitor -> {
							// Begin a new task
							monitor.beginTask("Synchronize...", Monitor.UNKNOWN);

							try {
								final var boundaryService = new BoundaryService(project);
								final Set<BoundaryBean> boundaries = boundaryService.synchBoundaries();

								EclipseIDEService.saveProjectMetaData(project);

								// Rebuild all source files
								for (final BoundaryBean boundary : boundaries)
									boundaryService.rebuildBoundarySourceFiles(boundary);
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
		}

		// Add a menu item to refresh the boundary namespace
		final var itemRefresh = new MenuItem(menuBoundary, SWT.NONE);
		itemRefresh.setText(TITLE_REFRESH);
		itemRefresh.setImage(CodeCadenzaResourcePlugin.getImage(IMG_REFRESH));

		itemRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.addNamespaceItems(getSelectedItem());
			}
		});

		return menuBoundary;
	}

}
