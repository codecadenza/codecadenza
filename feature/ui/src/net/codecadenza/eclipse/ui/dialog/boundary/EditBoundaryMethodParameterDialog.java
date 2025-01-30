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

import java.util.Arrays;
import java.util.Collection;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining boundary method parameters
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditBoundaryMethodParameterDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit method parameter";

	private final Project project;
	private final MethodParameter methodParameter;
	private DataComboViewer<JavaType> cboJavaType;
	private Text txtName;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param methodParameter
	 */
	public EditBoundaryMethodParameterDialog(Shell parentShell, Project project, MethodParameter methodParameter) {
		super(parentShell);

		this.project = project;
		this.methodParameter = methodParameter;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		final GridData gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 300;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(gdName);

		final var lblJavaType = new Label(panDialogArea, SWT.NONE);
		lblJavaType.setText("Java type:");

		cboJavaType = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(JavaType type) {
				return type.getName();
			}
		};

		cboJavaType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblParameterModifier = new Label(panDialogArea, SWT.NONE);
		lblParameterModifier.setText("Parameter modifier:");

		final var txtParameterModifier = new Text(panDialogArea, SWT.BORDER);
		txtParameterModifier.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtParameterModifier.setEditable(false);
		txtParameterModifier.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));

		// Initialize dialog controls
		txtName.setText(methodParameter.getName());

		if (methodParameter.getType() != null) {
			final Collection<JavaType> items;

			if (methodParameter.getType() instanceof final DTOBean dto) {
				final EList<DTOBean> availableDTOs = project.getDTOsOfDomainObject(dto.getDomainObject());

				items = availableDTOs.stream().map(JavaType.class::cast).toList();
			}
			else {
				items = Arrays.asList(methodParameter.getType());

				// The return type must not be changed if doesn't represent a DTO!
				cboJavaType.setEnabled(false);
			}

			cboJavaType.setData(items);
			cboJavaType.setSelectedItem(methodParameter.getType());
		}

		txtParameterModifier.setText(methodParameter.getModifier().name());

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the input
			if (cboJavaType.getSelectedItem() == null) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "A Java type must be selected!");
				cboJavaType.setFocus();
				return;
			}

			final IStatus status = EclipseIDEService.validateTypeVariableName(txtName.getText());

			if (!status.isOK()) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				return;
			}

			// Apply the changes
			methodParameter.setType(cboJavaType.getSelectedItem());
			methodParameter.setName(txtName.getText());
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(DLG_TITLE);
		newShell.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_LOCAL_VARIABLE));
	}

}
