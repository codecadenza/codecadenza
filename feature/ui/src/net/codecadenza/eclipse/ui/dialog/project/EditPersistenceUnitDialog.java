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
package net.codecadenza.eclipse.ui.dialog.project;

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.IProjectBuildService;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for maintaining the persistence unit
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditPersistenceUnitDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit persistence unit properties";

	private final Project project;
	private DataGridComposite<PersistenceUnitProperty> gridProperties;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public EditPersistenceUnitDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 3);

		final var lblProperties = new Label(panDialogArea, SWT.NONE);
		lblProperties.setText("Properties:");

		new Label(panDialogArea, SWT.NONE);
		new Label(panDialogArea, SWT.NONE);

		final var gdProperties = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdProperties.heightHint = 400;

		gridProperties = new DataGridComposite<>(panDialogArea, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(PersistenceUnitProperty element, int columnIndex) {
				if (columnIndex == 0)
					return element.getName();

				return element.getValue();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(PersistenceUnitProperty element) {
				editProperty();
			}
		};

		gridProperties.setLayoutData(gdProperties);
		gridProperties.addColumn("Property name", ColumnSortType.STRING, 250);
		gridProperties.addColumn("Property value", ColumnSortType.STRING, 200);
		gridProperties.setData(project.getPersistenceUnitProperties());

		final var panButtons = new Composite(panDialogArea, SWT.NONE);
		panButtons.setLayoutData(new GridData(SWT.CENTER, SWT.TOP, false, false));
		panButtons.setLayout(new GridLayout());

		final var cmdAdd = new Button(panButtons, SWT.NONE);
		cmdAdd.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdAdd.setText("Add");

		cmdAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Shell shell = Display.getCurrent().getActiveShell();
				final var dlg = new EditPersistenceUnitPropertyDialog(shell, project, null);

				if (dlg.open() != Dialog.OK)
					return;

				project.getPersistenceUnitProperties().add(dlg.getProperty());

				saveProject();
			}
		});

		final var cmdEdit = new Button(panButtons, SWT.NONE);
		cmdEdit.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdEdit.setText("Edit");

		cmdEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editProperty();
			}
		});

		final var cmdRemove = new Button(panButtons, SWT.NONE);
		cmdRemove.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdRemove.setText("Remove");

		cmdRemove.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final PersistenceUnitProperty property = gridProperties.getSelection();

				if (property == null)
					return;

				// Remove the persistence unit property
				project.getPersistenceUnitProperties().remove(property);

				saveProject();
			}
		});

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(DLG_TITLE);
	}

	/**
	 * Save the project and rebuild the persistence.xml file
	 */
	private void saveProject() {
		try {
			EclipseIDEService.saveProjectMetaData(project);

			final IProjectBuildService projectBuildService = ProjectBuildFactory.getBuildService(project);

			if (project.isSpringBootApplication())
				projectBuildService.rebuildSpringBootConfigurationFiles();
			else
				projectBuildService.rebuildPersistenceUnit();
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}

		// Refresh the data grid
		gridProperties.refresh();
	}

	/**
	 * Edit the selected persistence unit property
	 */
	private void editProperty() {
		final PersistenceUnitProperty selectedProperty = gridProperties.getSelection();

		if (selectedProperty == null)
			return;

		final Shell shell = Display.getCurrent().getActiveShell();
		final var dlg = new EditPersistenceUnitPropertyDialog(shell, project, selectedProperty);

		if (dlg.open() != Dialog.OK)
			return;

		saveProject();
	}

}
