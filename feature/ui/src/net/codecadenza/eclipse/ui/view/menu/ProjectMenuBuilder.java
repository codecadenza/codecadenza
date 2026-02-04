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

import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.service.security.SecurityService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.project.EditDataSourceDialog;
import net.codecadenza.eclipse.ui.dialog.project.EditPersistenceUnitDialog;
import net.codecadenza.eclipse.ui.dialog.project.EditProjectDialog;
import net.codecadenza.eclipse.ui.dialog.project.EditRolesDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectMenuBuilder extends AbstractMenuBuilder<Project> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public ProjectMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuProject = new Menu(tree);

		new ProjectLaunchMenuBuilder(menuProject, view, tree).createMenu();
		new MenuItem(menuProject, SWT.SEPARATOR);

		// Add a menu item to edit the project settings
		final var itemEditProject = new MenuItem(menuProject, SWT.NONE);
		itemEditProject.setText(TITLE_EDIT_PROJECT);

		itemEditProject.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				new EditProjectDialog(shell, getSelectedObject()).open();

				view.refreshTree();
			}
		});

		// Add a menu item to edit the data source
		final var itemEditDataSource = new MenuItem(menuProject, SWT.NONE);
		itemEditDataSource.setText(TITLE_EDIT_DATA_SOURCE);

		itemEditDataSource.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				new EditDataSourceDialog(shell, getSelectedObject()).open();
			}
		});

		// Add a menu item to edit the persistence unit properties
		final var itemEditPU = new MenuItem(menuProject, SWT.NONE);
		itemEditPU.setText(TITLE_EDIT_PU);

		itemEditPU.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				new EditPersistenceUnitDialog(shell, getSelectedObject()).open();
			}
		});

		// Add a menu item to edit the roles
		final var itemEditRoles = new MenuItem(menuProject, SWT.NONE);
		itemEditRoles.setText(TITLE_EDIT_ROLES);

		itemEditRoles.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				new EditRolesDialog(shell, getSelectedObject()).open();
			}
		});

		new MenuItem(menuProject, SWT.SEPARATOR);

		// Add a menu item to rebuild the logging service source files
		final var itemRebuildLogging = new MenuItem(menuProject, SWT.NONE);
		itemRebuildLogging.setText(TITLE_REBUILD_LOG);

		itemRebuildLogging.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					ProjectBuildFactory.getBuildService(getSelectedObject()).rebuildLoggingService();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild all security-related files
		final var itemRebuildSecurity = new MenuItem(menuProject, SWT.NONE);
		itemRebuildSecurity.setText(TITLE_REBUILD_SEC);

		itemRebuildSecurity.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final Project project = getSelectedObject();
					final DTOBean logOnDTO = project.getApplicationLogOnDTO();

					if (logOnDTO == null) {
						final var msg = "The security-related sources files cannot be rebuilt as no security DTO is defined!";

						MessageDialog.openInformation(shell, TITLE_REBUILD_SEC, msg);
						return;
					}

					ProjectBuildFactory.getBuildService(project).rebuildSecurity();

					if (project.getBoundaryByDomainObject(logOnDTO.getDomainObject()) != null)
						new BoundaryService(project)
								.rebuildBoundarySourceFiles(project.getBoundaryByDomainObject(logOnDTO.getDomainObject()));
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild the source files of the service for saved queries
		final var itemRebuildQueryService = new MenuItem(menuProject, SWT.NONE);
		itemRebuildQueryService.setText(TITLE_REBUILD_SAVED_QUERY);

		itemRebuildQueryService.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final Project project = getSelectedObject();

					if (!project.hasJSFOrVaadinClient()) {
						MessageDialog.openInformation(shell, TITLE_REBUILD_SAVED_QUERY,
								"This operation is not supported for the given client technology!");
						return;
					}

					ProjectBuildFactory.getBuildService(project).rebuildSavedQueryService();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild all objects that belong to the selected project
		final var itemRebuildAll = new MenuItem(menuProject, SWT.NONE);
		itemRebuildAll.setText(TITLE_REBUILD_ALL);

		itemRebuildAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				rebuildAllObjects(getSelectedObject());
			}
		});

		new MenuItem(menuProject, SWT.SEPARATOR);

		// Add a menu item to create the log-on DTO
		final var itemCreateLogOnDTO = new MenuItem(menuProject, SWT.NONE);
		itemCreateLogOnDTO.setText(TITLE_CREATE_LOG_ON);

		itemCreateLogOnDTO.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				try {
					final Project project = getSelectedObject();

					new SecurityService(project).initSecurity();

					// Rebuild all files
					rebuildAllObjects(project);

					view.refreshTree();
				}
				catch (final IllegalStateException ex) {
					MessageDialog.openInformation(shell, TITLE_CREATE_LOG_ON, ex.getMessage());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuProject;
	}

	/**
	 * Rebuild all objects of this project
	 * @param project
	 */
	private void rebuildAllObjects(final Project project) {
		new Job("Rebuilding all existing objects...") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					ProjectBuildFactory.getBuildService(project).rebuildAllObjects(monitor);
				}
				catch (final Exception ex) {
					shell.getDisplay().syncExec(() -> CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex));
				}

				monitor.done();

				return Status.OK_STATUS;
			}
		}.schedule();
	}

}
