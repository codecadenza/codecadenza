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
package net.codecadenza.eclipse.ui.dialog.boundary;

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for creating new boundary beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class NewBoundaryBeanDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Create new boundary bean";

	private final Project project;
	private DomainObjectProposalTextField txtDomainObject;
	private BoundaryBean boundaryBean;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public NewBoundaryBeanDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblDomainObj = new Label(panDialogArea, SWT.NONE);
		lblDomainObj.setText("Domain object:");

		final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdDomainObject.widthHint = 200;

		txtDomainObject = new DomainObjectProposalTextField(panDialogArea, project);
		txtDomainObject.setLayoutData(gdDomainObject);

		return panDialogArea;
	}

	/**
	 * @return the boundaryBean
	 */
	public BoundaryBean getBoundary() {
		return boundaryBean;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !writeInputsToObject())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Save dialog input data
	 * @return true if the validation was successful
	 */
	private boolean writeInputsToObject() {
		final DomainObject domainObject = txtDomainObject.getSelectedItem();

		if (domainObject == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A domain object must be selected!");
			return false;
		}

		// Test if the boundary bean for this domain object already exists
		if (project.getBoundaryByDomainObject(domainObject) != null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A boundary bean for the selected domain object already exists!");
			return false;
		}

		try {
			// Create the boundary bean
			boundaryBean = new BoundaryService(project).getOrCreateBoundaryOfDomainObject(domainObject);

			final Resource eResource = boundaryBean.getNamespace().eResource();

			eResource.getContents().add(boundaryBean);

			if (!eResource.getContents().contains(boundaryBean.getRepository()))
				eResource.getContents().add(boundaryBean.getRepository());

			EclipseIDEService.saveProjectMetaData(project);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
			return false;
		}

		return true;
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

}
