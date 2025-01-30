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
package net.codecadenza.eclipse.ui.dialog.client;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormBuildConfiguration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for creating forms and grid panels of a selected domain object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateDefaultFormsDialog extends CodeCadenzaDialog {
	private final Project project;
	private Label lblCreateFormType;
	private Combo cboCreateFormType;
	private Label lblViewFormType;
	private Combo cboViewFormType;
	private Label lblLOV;
	private Button chkCreateLOV;
	private Label lblUpdateForm;
	private Button chkCreateUpdateForm;
	private Label lblReadOnlyForm;
	private Button chkCreateReadOnlyForm;
	private Label lblGridPanels;
	private Button chkCreateGridPanels;
	private Label lblShareDTOs;
	private Button chkShareDTOs;
	private FormBuildConfiguration configuration;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public CreateDefaultFormsDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/**
	 * @return the configuration
	 */
	public FormBuildConfiguration getConfiguration() {
		return configuration;
	}

	/**
	 * @param domainObject
	 */
	protected void initConfiguration(DomainObject domainObject) {
		configuration = new FormBuildConfiguration();
		configuration.setDomainObject(domainObject);
		configuration.setShareDTOs(project.isBoundaryMode());

		final Form addForm = getForm(domainObject, FormTypeEnumeration.ADD);
		final Form createForm = getForm(domainObject, FormTypeEnumeration.CREATE);
		final Form simpleView = getForm(domainObject, FormTypeEnumeration.SIMPLE_VIEW);
		final Form searchableView = getForm(domainObject, FormTypeEnumeration.SEARCHABLE_VIEW);

		if (simpleView == null && searchableView == null)
			configuration.setViewFormType(FormTypeEnumeration.SEARCHABLE_VIEW);

		if (addForm == null && createForm == null && !domainObject.isAbstract())
			configuration.setCreateFormType(FormTypeEnumeration.CREATE);

		if (getForm(domainObject, FormTypeEnumeration.UPDATE) == null && !domainObject.isAbstract())
			configuration.setCreateUpdateForm(true);

		if (getForm(domainObject, FormTypeEnumeration.READONLY) == null)
			configuration.setCreateReadOnlyForm(true);

		if (getForm(domainObject, FormTypeEnumeration.LOV) == null)
			configuration.setCreateLOV(true);

		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
				// Check if a grid panel for the selected association already exists!
				final boolean panelExists = project.getAllGridPanelsOfProject().stream()
						.anyMatch(gridPanel -> assoc.equals(gridPanel.getAssociation()));

				if (!panelExists)
					configuration.addGridPanelAssociation(assoc);
			}
			else if (assoc instanceof ManyToOneAssociation && !assoc.isOwner() && configuration.getCreateFormType() != null)
				configuration.setCreateFormType(FormTypeEnumeration.ADD);
	}

	/**
	 * @param domainObject
	 * @param formType
	 * @return a form that matches the given filter criteria or null if no form could be found
	 */
	protected Form getForm(DomainObject domainObject, FormTypeEnumeration formType) {
		return project.getAllFormsOfProject().stream()
				.filter(f -> f.getFormType() == formType && f.getDomainObject().equals(domainObject)).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblDomainObject = new Label(panDialogArea, SWT.NONE);
		lblDomainObject.setText("Select a domain object:");

		final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdDomainObject.minimumWidth = 300;

		final var propDomainObject = new DomainObjectProposalTextField(panDialogArea, project) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
			 */
			@Override
			public void onProposalAccepted(DomainObject domainObject) {
				try {
					// Initialize the dialog
					initDialog(domainObject);
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		};

		propDomainObject.setLayoutData(gdDomainObject);

		lblCreateFormType = new Label(panDialogArea, SWT.NONE);
		lblCreateFormType.setText("Create form type:");
		lblCreateFormType.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));

		cboCreateFormType = new Combo(panDialogArea, SWT.READ_ONLY);
		cboCreateFormType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblViewFormType = new Label(panDialogArea, SWT.NONE);
		lblViewFormType.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblViewFormType.setText("View form type:");

		cboViewFormType = new Combo(panDialogArea, SWT.READ_ONLY);
		cboViewFormType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblLOV = new Label(panDialogArea, SWT.NONE);
		lblLOV.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblLOV.setText("Create list of values:");

		chkCreateLOV = new Button(panDialogArea, SWT.CHECK);
		chkCreateLOV.setEnabled(false);

		lblUpdateForm = new Label(panDialogArea, SWT.NONE);
		lblUpdateForm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblUpdateForm.setText("Create update form:");

		chkCreateUpdateForm = new Button(panDialogArea, SWT.CHECK);
		chkCreateUpdateForm.setEnabled(false);

		lblReadOnlyForm = new Label(panDialogArea, SWT.NONE);
		lblReadOnlyForm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblReadOnlyForm.setText("Create read-only form:");

		chkCreateReadOnlyForm = new Button(panDialogArea, SWT.CHECK);
		chkCreateReadOnlyForm.setEnabled(false);

		lblGridPanels = new Label(panDialogArea, SWT.NONE);
		lblGridPanels.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblGridPanels.setText("Create grid panels:");

		chkCreateGridPanels = new Button(panDialogArea, SWT.CHECK);
		chkCreateGridPanels.setEnabled(false);

		if (project.isBoundaryMode()) {
			lblShareDTOs = new Label(panDialogArea, SWT.NONE);
			lblShareDTOs.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
			lblShareDTOs.setText("Share DTOs:");

			chkShareDTOs = new Button(panDialogArea, SWT.CHECK);
			chkShareDTOs.setEnabled(false);
		}

		return panDialogArea;
	}

	/**
	 * Apply the configuration
	 */
	private void applyConfiguration() {
		configuration.setViewFormType(FormTypeEnumeration.getByName(cboViewFormType.getItem(cboViewFormType.getSelectionIndex())));
		configuration
				.setCreateFormType(FormTypeEnumeration.getByName(cboCreateFormType.getItem(cboCreateFormType.getSelectionIndex())));

		if (chkCreateUpdateForm.isEnabled())
			configuration.setCreateUpdateForm(chkCreateUpdateForm.getSelection());

		if (chkCreateReadOnlyForm.isEnabled())
			configuration.setCreateReadOnlyForm(chkCreateReadOnlyForm.getSelection());

		if (chkCreateLOV.isEnabled())
			configuration.setCreateLOV(chkCreateLOV.getSelection());

		if (chkCreateGridPanels.isEnabled())
			configuration.setCreateGridPanels(chkCreateGridPanels.getSelection());

		if (project.isBoundaryMode())
			configuration.setShareDTOs(chkShareDTOs.getSelection());
	}

	/**
	 * Initialize the dialog
	 * @param domainObject
	 */
	private void initDialog(DomainObject domainObject) {
		initConfiguration(domainObject);

		cboViewFormType.removeAll();
		cboCreateFormType.removeAll();
		chkCreateUpdateForm.setSelection(false);
		chkCreateUpdateForm.setEnabled(false);
		chkCreateReadOnlyForm.setSelection(false);
		chkCreateReadOnlyForm.setEnabled(false);
		chkCreateLOV.setSelection(false);
		chkCreateLOV.setEnabled(false);
		chkCreateGridPanels.setSelection(false);
		chkCreateGridPanels.setEnabled(false);
		lblViewFormType.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblCreateFormType.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblLOV.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblUpdateForm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblReadOnlyForm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		lblGridPanels.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));

		if (project.isBoundaryMode())
			lblShareDTOs.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));

		// Add empty items
		cboViewFormType.add("");
		cboCreateFormType.add("");

		if (configuration.getViewFormType() != null) {
			cboViewFormType.add(FormTypeEnumeration.SEARCHABLE_VIEW.name());
			cboViewFormType.add(FormTypeEnumeration.SIMPLE_VIEW.name());
			cboViewFormType.select(1);

			lblViewFormType.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}
		else
			cboViewFormType.select(0);

		if (configuration.getCreateFormType() != null) {
			if (configuration.getCreateFormType() == FormTypeEnumeration.ADD)
				cboCreateFormType.add(FormTypeEnumeration.ADD.name());

			cboCreateFormType.add(FormTypeEnumeration.CREATE.name());
			cboCreateFormType.select(1);

			lblCreateFormType.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}
		else
			cboCreateFormType.select(0);

		if (configuration.isCreateUpdateForm()) {
			chkCreateUpdateForm.setSelection(true);
			chkCreateUpdateForm.setEnabled(true);

			lblUpdateForm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}

		if (configuration.isCreateReadOnlyForm()) {
			chkCreateReadOnlyForm.setSelection(true);
			chkCreateReadOnlyForm.setEnabled(true);

			lblReadOnlyForm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}

		if (configuration.isCreateLOV()) {
			chkCreateLOV.setEnabled(true);

			lblLOV.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}

		if (configuration.isCreateGridPanels()) {
			chkCreateGridPanels.setSelection(true);
			chkCreateGridPanels.setEnabled(true);

			lblGridPanels.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}

		if (project.isBoundaryMode()) {
			final boolean allowDTOSharing = domainObject.isDTOSharingAllowed();

			chkShareDTOs.setSelection(allowDTOSharing);
			chkShareDTOs.setEnabled(allowDTOSharing);

			if (allowDTOSharing)
				lblShareDTOs.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText("Create default forms");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (configuration == null)
				return;

			applyConfiguration();
		}

		super.buttonPressed(buttonId);
	}

}
