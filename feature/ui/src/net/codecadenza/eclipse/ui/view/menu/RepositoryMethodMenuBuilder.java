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
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.boundary.EditServiceMethodDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
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
 * Helper class to create a context menu for tree view items that represent repository methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryMethodMenuBuilder extends AbstractMenuBuilder<RepositoryMethod> {
	private final boolean boundaryMode;

	/**
	 * Constructor
	 * @param view
	 * @param tree
	 * @param boundaryMode
	 */
	public RepositoryMethodMenuBuilder(ProjectView view, Tree tree, boolean boundaryMode) {
		super(view, tree);

		this.boundaryMode = boundaryMode;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final Menu menuRepositoryMethod = new Menu(tree);

		if (!boundaryMode) {
			// Add a menu item to edit the selected repository method
			final var itemEdit = new MenuItem(menuRepositoryMethod, SWT.NONE);
			itemEdit.setText(TITLE_EDIT);

			itemEdit.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (EclipseIDEService.openDiagramExists(view))
						return;

					final RepositoryMethod method = getSelectedObject();

					if (method.isGenerationOmitted()) {
						MessageDialog.openInformation(shell, TITLE_EDIT, "The selected method cannot be changed as it is an API method!");
						return;
					}

					final Project project = method.getRepository().getNamespace().getProject();

					// Open a dialog to edit the selected repository method
					final var dlg = new EditServiceMethodDialog(shell, method, project);

					if (dlg.open() != Dialog.OK)
						return;

					try {
						final var boundaryService = new BoundaryService(project);

						// Save changes
						EclipseIDEService.saveProjectMetaData(project);

						// Rebuild the respective source files
						if (project.getBoundaryByDomainObject(method.getRepository().getDomainObject()) != null)
							boundaryService
									.rebuildBoundarySourceFiles(project.getBoundaryByDomainObject(method.getRepository().getDomainObject()));
					}
					catch (final Exception ex) {
						CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					}
				}
			});
		}

		// Add a menu item to delete the selected repository method
		final var itemDelete = new MenuItem(menuRepositoryMethod, SWT.NONE);
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

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected method?"))
					return;

				try {
					final var repositoryService = new RepositoryService(getSelectedObject().getRepository().getNamespace().getProject());
					repositoryService.removeRepositoryMethod(getSelectedObject());

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuRepositoryMethod;
	}

}
