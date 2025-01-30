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

import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent repositories
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryMenuBuilder extends AbstractMenuBuilder<Repository> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public RepositoryMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuRepository = new Menu(tree);

		// Add a menu item to open the repository source file
		final var itemOpen = new MenuItem(menuRepository, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openRepositoryInEditor(getSelectedItem());
			}
		});

		// Add a menu item to rename the selected repository
		final var itemRename = new MenuItem(menuRepository, SWT.NONE);
		itemRename.setText(TITLE_RENAME);

		itemRename.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Repository repository = getSelectedObject();
				final Project project = repository.getNamespace().getProject();
				final String repositoryName = repository.getName();

				final var dlg = new InputDialog(shell, TITLE_RENAME, "Enter a new repository name:", repositoryName, null);

				if (dlg.open() != Window.OK)
					return;

				if (dlg.getValue().equals(repositoryName))
					return;

				// Validate the name
				final IStatus status = EclipseIDEService.validateJavaTypeName(dlg.getValue());

				if (status.getSeverity() > IStatus.INFO) {
					MessageDialog.openInformation(shell, TITLE_RENAME, status.getMessage());
					return;
				}

				try {
					EclipseIDEService.renameCompUnit(repository.getSourceFile(), dlg.getValue());

					repository.setName(dlg.getValue());

					// Save meta-model changes
					EclipseIDEService.saveProjectMetaData(project);

					getSelectedItem().setText(dlg.getValue());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild the repository source file
		final var itemRebuild = new MenuItem(menuRepository, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final var repositoryService = new RepositoryService(getSelectedObject().getNamespace().getProject());
					repositoryService.rebuildRepositorySourceFiles(getSelectedObject());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to delete the selected repository
		final var itemDelete = new MenuItem(menuRepository, SWT.NONE);
		itemDelete.setText(TITLE_DELETE);
		itemDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));

		itemDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected repository?"))
					return;

				try {
					final var repositoryService = new RepositoryService(getSelectedObject().getNamespace().getProject());
					repositoryService.removeRepository(getSelectedObject());

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuRepository;
	}

}
