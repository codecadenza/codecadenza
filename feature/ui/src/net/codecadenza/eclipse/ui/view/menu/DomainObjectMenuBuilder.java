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
import static net.codecadenza.eclipse.shared.Constants.JPA_FOLDER;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.jpaeditor.JPAQueryEditor;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.view.ProjectView;
import net.codecadenza.eclipse.ui.view.util.ProjectTreeViewHelper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectMenuBuilder extends AbstractMenuBuilder<DomainObject> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public DomainObjectMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuDomainObject = new Menu(tree);

		// Add a menu item to open the domain object source file in the editor
		final var itemOpen = new MenuItem(menuDomainObject, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openDomainObjectInEditor(getSelectedItem());
			}
		});

		// Add a menu item to perform a JPA query based on the selected domain object
		final var itemJPA = new MenuItem(menuDomainObject, SWT.NONE);
		itemJPA.setText(TITLE_QUERY);

		itemJPA.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DomainObject domainObject = getSelectedObject();

				if (domainObject.isMappedSuperClass()) {
					MessageDialog.openInformation(shell, TITLE_QUERY, "JPA queries based on mapped superclasses are not possible!");
					return;
				}

				final Project project = domainObject.getNamespace().getProject();

				try {
					// Check if the JPA folder exists and create it if necessary
					final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
					final IProject proj = wsRoot.getProject(project.getTargetProjectName(BuildArtifactType.DOMAIN));
					final IFolder jpaFolder = proj.getFolder(JPA_FOLDER);

					if (!jpaFolder.exists())
						jpaFolder.create(true, true, null);

					// Avoid duplicate file names by using a counter!
					int counter = 1;

					while (true) {
						final var name = domainObject.getName() + "-" + counter + ".jpa";
						IFile jpaFile = jpaFolder.getFile(name);

						if (jpaFile.exists())
							counter++;
						else {
							final var path = JPA_FOLDER + "/" + name;
							final var content = "select a from " + domainObject.getName() + " a";
							final var workspaceFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, content);

							jpaFile = EclipseIDEService.createOrUpdateFile(workspaceFile);

							final var input = new FileEditorInput(jpaFile);
							view.getViewSite().getWorkbenchWindow().getActivePage().openEditor(input, JPAQueryEditor.ID);
							break;
						}
					}
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild the domain object source files
		final var itemRebuild = new MenuItem(menuDomainObject, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final var domainObjectService = new DomainObjectService(getSelectedObject().getNamespace().getProject());
					domainObjectService.rebuildDomainObjectSourceFiles(getSelectedObject(), true);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to refresh the domain object
		final var itemRefresh = new MenuItem(menuDomainObject, SWT.NONE);
		itemRefresh.setText(TITLE_REFRESH);
		itemRefresh.setImage(CodeCadenzaResourcePlugin.getImage(IMG_REFRESH));

		itemRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DomainObject domainObject = getSelectedObject();

				view.addDomainObjectChildItems(getSelectedItem(), domainObject);

				// Determine the domain object as loaded
				ProjectTreeViewHelper.setJavaTypeLoaded(domainObject.hashCode());
			}
		});

		return menuDomainObject;
	}

}
