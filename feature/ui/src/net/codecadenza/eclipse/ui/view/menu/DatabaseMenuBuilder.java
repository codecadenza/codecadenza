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

import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.tools.dbsync.editor.DBSyncEditor;
import net.codecadenza.eclipse.tools.dbsync.editor.DBSynchEditorInput;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.db.EditDatabasePropertiesDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PartInitException;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent database objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DatabaseMenuBuilder extends AbstractMenuBuilder<Database> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public DatabaseMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuDB = new Menu(tree);

		// Add a menu item to perform the database synchronization
		final var itemSync = new MenuItem(menuDB, SWT.NONE);
		itemSync.setText(TITLE_SYNC);

		itemSync.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Database db = getSelectedObject();
				final Project project = db.getProject();
				final var input = new DBSynchEditorInput(db, project.getDataSource());

				try {
					view.getViewSite().getWorkbenchWindow().getActivePage().openEditor(input, DBSyncEditor.ID);
				}
				catch (final PartInitException ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item for maintaining database properties
		final var itemProperties = new MenuItem(menuDB, SWT.NONE);
		itemProperties.setText(TITLE_PROP);

		itemProperties.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Database db = getSelectedObject();
				final IdentifierStyleEnumeration oldStyle = db.getIdentifierStyle();
				final String oldSchemaName = db.getSchemaName();
				final String oldCatalogName = db.getCatalogName();
				boolean rebuildORMFile = false;

				final var dlg = new EditDatabasePropertiesDialog(view.getSite().getShell(), db);

				if (dlg.open() == Window.CANCEL)
					return;

				// All domain object source files and the orm.xml file must be rebuilt if identifier style was changed!
				if (oldStyle != db.getIdentifierStyle()) {
					rebuildORMFile = true;

					// Rebuild all domain object source files!
					for (final Namespace namespace : db.getProject().getDomainNamespace().getChildNamespaces()) {
						final var job = new Job("Building objects of namespace " + namespace.getName()) {
							/*
							 * (non-Javadoc)
							 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
							 */
							@Override
							protected IStatus run(IProgressMonitor monitor) {
								try {
									ProjectBuildFactory.getBuildService(namespace.getProject()).buildObjectsOfNamespace(namespace, false, monitor);
								}
								catch (final Exception e) {
									shell.getDisplay().syncExec(() -> CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e));
								}

								monitor.done();

								return Status.OK_STATUS;
							}
						};

						job.schedule();
					}

					view.refreshTree();
				}

				// If the schema or the catalog name have been changed the orm.xml file must be rebuilt also!
				if (oldSchemaName == null) {
					if (!db.getSchemaName().isEmpty())
						rebuildORMFile = true;
				}
				else if (!oldSchemaName.equals(db.getSchemaName()))
					rebuildORMFile = true;

				if (oldCatalogName == null) {
					if (!db.getCatalogName().isEmpty())
						rebuildORMFile = true;
				}
				else if (!oldCatalogName.equals(db.getCatalogName()))
					rebuildORMFile = true;

				if (rebuildORMFile)
					try {
						ProjectBuildFactory.getBuildService(db.getProject()).rebuildORMXML();
					}
					catch (final Exception ex) {
						CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
					}
			}
		});

		return menuDB;
	}

}
