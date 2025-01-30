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

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.Arrays;
import java.util.Collection;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.util.method.MethodHelper;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining boundary methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditBoundaryMethodDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE_BOUNDARY = "Edit boundary method";
	private static final String DLG_TITLE_FACADE = "Edit facade method";

	private final BoundaryMethod boundaryMethod;
	private final BoundaryBean boundary;
	private final Project project;
	private Text txtComment;
	private DataComboViewer<JavaType> cboReturnType;
	private DataGridComposite<MethodParameter> gridParameters;
	private Text txtName;
	private String title = DLG_TITLE_FACADE;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param boundary
	 * @param boundaryMethod
	 */
	public EditBoundaryMethodDialog(Shell parentShell, BoundaryBean boundary, BoundaryMethod boundaryMethod) {
		super(parentShell);

		this.boundary = boundary;
		this.boundaryMethod = boundaryMethod;
		this.project = boundary.getDomainObject().getNamespace().getProject();

		if (this.project.isBoundaryMode())
			title = DLG_TITLE_BOUNDARY;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtName.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				checkMethodSignature();
			}
		});

		final var lblReturnType = new Label(panDialogArea, SWT.NONE);
		lblReturnType.setText("Return type:");

		cboReturnType = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(JavaType type) {
				return type.getName();
			}
		};

		cboReturnType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblRetTypeModifier = new Label(panDialogArea, SWT.NONE);
		lblRetTypeModifier.setText("Return type modifier:");

		final var txtRetTypeModifier = new Text(panDialogArea, SWT.BORDER);
		txtRetTypeModifier.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRetTypeModifier.setEditable(false);
		txtRetTypeModifier.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));

		final var lblComment = new Label(panDialogArea, SWT.NONE);
		lblComment.setText("Method comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 2);
		gdComment.heightHint = 50;

		txtComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(gdComment);

		new Label(panDialogArea, SWT.NONE);

		final var lblParameters = new Label(panDialogArea, SWT.NONE);
		lblParameters.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		lblParameters.setText("Parameters:");

		gridParameters = new DataGridComposite<>(panDialogArea, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(MethodParameter parameter, int columnIndex) {
				if (columnIndex == 0) {
					if (parameter.getType() != null) {
						if (parameter.getModifier() == JavaTypeModifierEnumeration.NONE)
							return parameter.getType().getName();

						return parameter.getModifier() + "<" + parameter.getType().getName() + ">";
					}

					return "Not specified";
				}
				else if (columnIndex == 1)
					return parameter.getName();

				return "";
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellForeground(java.lang.Object,
			 * int)
			 */
			@Override
			public Color getCellForeground(MethodParameter parameter, int columnIndex) {
				if (columnIndex == 0 && parameter.getType() == null)
					return Display.getCurrent().getSystemColor(SWT.COLOR_RED);

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(MethodParameter selectedParameter) {
				if (selectedParameter == null)
					return;

				final var dlg = new EditBoundaryMethodParameterDialog(getShell(), project, selectedParameter);

				if (dlg.open() == Dialog.OK) {
					gridParameters.refresh();

					checkMethodSignature();
				}
			}
		};

		final var gdParameters = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdParameters.heightHint = 150;

		gridParameters.addColumn("Parameter type", ColumnSortType.STRING, 200);
		gridParameters.addColumn("Parameter name", ColumnSortType.STRING, 200);
		gridParameters.setData(boundaryMethod.getMethodParameters());
		gridParameters.setLayoutData(gdParameters);

		// Fill the dialog
		txtName.setText(boundaryMethod.getName());
		txtRetTypeModifier.setText(boundaryMethod.getReturnTypeModifier().name());

		if (boundaryMethod.getComment() != null)
			txtComment.setText(boundaryMethod.getComment());

		if (boundaryMethod.getReturnType() != null) {
			final Collection<JavaType> items;

			if (boundaryMethod.getReturnType() instanceof final DTOBean dto) {
				final EList<DTOBean> availableDTOs = project.getDTOsOfDomainObject(dto.getDomainObject());

				// A manually created 'SEARCH' method must be supplied with a manually created DTO that in turn doesn't use standard
				// conversion!
				if (boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH)
					items = availableDTOs.stream().filter(e -> !e.isStandardConversion() && e.isCreatedManually()).map(JavaType.class::cast)
							.toList();
				else
					items = availableDTOs.stream().map(JavaType.class::cast).toList();
			}
			else {
				items = Arrays.asList(boundaryMethod.getReturnType());

				// The return type must not be changed if it doesn't represent a DTO!
				cboReturnType.setEnabled(false);
			}

			cboReturnType.setData(items);
			cboReturnType.setSelectedItem(boundaryMethod.getReturnType());
		}

		checkMethodSignature();
		setTitle(title);

		return panDialogArea;
	}

	/**
	 * @return true if no other method with the same signature exists
	 */
	private boolean checkMethodSignature() {
		final String methodIdentifier = MethodHelper.generateMethodIdentifier(txtName.getText(),
				boundaryMethod.getMethodParameters());

		if (methodIdentifier == null) {
			this.setErrorMessage("The method signature could not be created!");
			return false;
		}

		// Test if another boundary method has the same signature!
		for (final BoundaryMethod method : boundary.getBoundaryMethods()) {
			if (method.equals(boundaryMethod))
				continue;

			final String identifierToCheck = MethodHelper.generateMethodIdentifier(method);

			if (methodIdentifier.equals(identifierToCheck)) {
				this.setErrorMessage("A method with the same signature already exists!");
				return false;
			}
		}

		this.setErrorMessage(null);
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(title);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !validateAndApplyInput())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Validate the user input and apply the changes to the boundary method
	 * @return true if the validation was successful
	 */
	private boolean validateAndApplyInput() {
		final IStatus status = EclipseIDEService.validateMethodName(txtName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			txtName.setFocus();
			return false;
		}

		if (cboReturnType.getSelectedItem() == null) {
			this.setErrorMessage("A return type must be selected!");
			cboReturnType.setFocus();
			return false;
		}

		if (!checkMethodSignature())
			return false;

		// The type of the first parameter and the return type must be equal for 'CREATE', 'UPDATE' and 'SAVE' methods!
		if (boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.UPDATE
				|| boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.CREATE
				|| boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.SAVE) {
			boolean firstParam = true;

			for (final MethodParameter param : boundaryMethod.getMethodParameters()) {
				if (!firstParam)
					break;

				if (!param.getType().equals(cboReturnType.getSelectedItem())) {
					this.setErrorMessage("The return type and the type of the first parameter must not be different!");
					return false;
				}

				firstParam = false;
			}
		}

		boundaryMethod.setComment(txtComment.getText());
		boundaryMethod.setReturnType(cboReturnType.getSelectedItem());
		boundaryMethod.setName(txtName.getText());

		return true;
	}

}
