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

import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE_NEW;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE_REPLACE;
import static net.codecadenza.eclipse.shared.Constants.QUOTE;
import static net.codecadenza.eclipse.shared.Constants.SQL_FOLDER;

import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.sqleditor.SQLEditor;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent database tables
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TableMenuBuilder extends AbstractMenuBuilder<DBTable> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public TableMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuTable = new Menu(tree);

		// Add a menu item to perform a SQL query based on the selected table
		final var itemQuery = new MenuItem(menuTable, SWT.NONE);
		itemQuery.setText(TITLE_QUERY);

		itemQuery.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DBTable table = getSelectedObject();
				final Project project = table.getDatabase().getProject();

				try {
					// Check if the SQL folder exists and create it if necessary
					final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
					final IProject proj = wsRoot.getProject(project.getTargetProjectName(BuildArtifactType.DOMAIN));
					final IFolder sqlFolder = proj.getFolder(SQL_FOLDER);

					if (!sqlFolder.exists())
						sqlFolder.create(true, true, null);

					// Avoid duplicate file names by using a counter!
					int counter = 1;

					while (true) {
						final var name = table.getName() + "-" + counter + ".sql";

						IFile sqlFile = sqlFolder.getFile(name);

						if (sqlFile.exists())
							counter++;
						else {
							final var path = SQL_FOLDER + "/" + name;
							final var content = "select * from " + table.getFullDatabaseName();
							final var workspaceFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, content);

							sqlFile = EclipseIDEService.createOrUpdateFile(workspaceFile);

							final var input = new FileEditorInput(sqlFile);
							view.getViewSite().getWorkbenchWindow().getActivePage().openEditor(input, SQLEditor.ID);
							break;
						}
					}
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		new MenuItem(menuTable, SWT.SEPARATOR);

		// Add a menu item to create a table
		final var itemCreateTable = new MenuItem(menuTable, SWT.NONE);
		itemCreateTable.setText(TITLE_CREATE);
		itemCreateTable.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE_NEW));

		itemCreateTable.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DBTable table = getSelectedObject();
				final Project project = table.getDatabase().getProject();

				try {
					new DBSynchService(project.getDatabase(), project.getDataSource()).createTable(table);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					return;
				}

				MessageDialog.openInformation(shell, TITLE_CREATE, "The table was created successfully!");
			}
		});

		// Add a menu item to recreate a table
		final var itemReCreateTable = new MenuItem(menuTable, SWT.NONE);
		itemReCreateTable.setText(TITLE_RECREATE);
		itemReCreateTable.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE_REPLACE));

		itemReCreateTable.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DBTable table = getSelectedObject();
				final Project project = table.getDatabase().getProject();

				try {
					new DBSynchService(project.getDatabase(), project.getDataSource()).recreateTable(table);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					return;
				}

				MessageDialog.openInformation(shell, TITLE_RECREATE, "The table was created successfully!");
			}
		});

		// Add a menu item to drop a table
		final var itemDropTable = new MenuItem(menuTable, SWT.NONE);
		itemDropTable.setText(TITLE_DROP);
		itemDropTable.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE_DELETE));

		itemDropTable.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DBTable table = getSelectedObject();
				final Project project = table.getDatabase().getProject();

				try {
					new DBSynchService(project.getDatabase(), project.getDataSource()).dropTable(table);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					return;
				}

				MessageDialog.openInformation(shell, TITLE_DROP, "The table was dropped successfully!");
			}
		});

		// Add a menu item to rename a table
		final var itemRenameTable = new MenuItem(menuTable, SWT.NONE);
		itemRenameTable.setText(TITLE_RENAME);

		itemRenameTable.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final DBTable table = getSelectedObject();
				final Project project = table.getDatabase().getProject();
				final String oldTableName = table.getName();
				final var domainObjectService = new DomainObjectService(project);

				final IInputValidator validator = newText -> {
					try {
						new DBSynchService(project.getDatabase()).validateTableName(table.getSchemaName(), table.getCatalogName(), newText);

						return null;
					}
					catch (final DBObjectValidationException e1) {
						return e1.getMessage();
					}
				};

				final var dlg = new InputDialog(shell, TITLE_RENAME, "Enter a new table name:", oldTableName, validator);

				if (dlg.open() != Window.OK)
					return;

				final String newTableName = dlg.getValue();

				try {
					shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

					table.setName(newTableName);

					new DBSynchService(project.getDatabase(), project.getDataSource()).renameTable(table, oldTableName, newTableName);

					for (final DomainObject domainObject : project.getAllDomainObjectsOfProject(false, true))
						if (domainObject.getDatabaseTable() != null && domainObject.getDatabaseTable().equals(table))
							domainObjectService.rebuildDomainObjectSourceFiles(domainObject, false);

					EclipseIDEService.saveProjectMetaData(table.eResource(), project);

					getSelectedItem().setText(table.getFullDatabaseName().replace(QUOTE, ""));
				}
				catch (final Exception ex) {
					shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_ARROW));

					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}

				shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_ARROW));
			}
		});

		return menuTable;
	}

}
