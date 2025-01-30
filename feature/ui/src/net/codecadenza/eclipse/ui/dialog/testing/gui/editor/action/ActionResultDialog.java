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
package net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action;

import java.util.ArrayList;
import java.util.Arrays;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType;
import net.codecadenza.eclipse.model.testing.GUITestActionStatus;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining the result of a GUI test action
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ActionResultDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit action result";

	private Button chkResult;
	private DataComboViewer<GUITestActionResultComponentType> cboComponentType;
	private Label lblMessage;
	private Text txtResultMessage;
	private Label lblStatus;
	private DataComboViewer<GUITestActionStatus> cboStatus;
	private Label lblOpen;
	private final GUITestAction testAction;
	private final Project project;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param testAction
	 */
	public ActionResultDialog(Shell parentShell, Project project, GUITestAction testAction) {
		super(parentShell);

		this.testAction = testAction;
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 3);

		chkResult = new Button(panDialogArea, SWT.CHECK);
		chkResult.setText("After performing the action a ");
		chkResult.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

		chkResult.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				cboComponentType.getComboViewer().getCombo().setEnabled(chkResult.getSelection());
				lblMessage.setEnabled(chkResult.getSelection());
				txtResultMessage.setEnabled(chkResult.getSelection());
				lblStatus.setEnabled(chkResult.getSelection());
				cboStatus.getComboViewer().getCombo().setEnabled(chkResult.getSelection());
				lblOpen.setEnabled(chkResult.getSelection());
			}
		});

		// Initialize the list with supported component types that can contain the result information
		final var supportedComponentTypes = new ArrayList<GUITestActionResultComponentType>();
		supportedComponentTypes.add(GUITestActionResultComponentType.DIALOG);

		// The component type 'NOTIFICATION' is not supported for Vaadin applications!
		if (!project.hasVaadinClient())
			supportedComponentTypes.add(GUITestActionResultComponentType.NOTIFICATION);

		cboComponentType = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(GUITestActionResultComponentType element) {
				return element.getName();
			}
		};

		cboComponentType.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		cboComponentType.getComboViewer().getCombo().setEnabled(false);
		cboComponentType.setData(supportedComponentTypes);
		cboComponentType.setSelectedItem(GUITestActionResultComponentType.DIALOG);

		lblMessage = new Label(panDialogArea, SWT.NONE);
		lblMessage.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		lblMessage.setText("with message");
		lblMessage.setEnabled(false);

		txtResultMessage = new Text(panDialogArea, SWT.BORDER);
		txtResultMessage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		txtResultMessage.setEnabled(false);

		lblStatus = new Label(panDialogArea, SWT.NONE);
		lblStatus.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		lblStatus.setText("and status");
		lblStatus.setEnabled(false);

		cboStatus = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(GUITestActionStatus element) {
				return element.getName();
			}
		};

		cboStatus.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboStatus.getComboViewer().getCombo().setEnabled(false);
		cboStatus.setData(Arrays.asList(GUITestActionStatus.values()).stream().toList());
		cboStatus.setSelectedItem(GUITestActionStatus.ERROR);

		lblOpen = new Label(panDialogArea, SWT.NONE);
		lblOpen.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		lblOpen.setText("will be opened");
		lblOpen.setEnabled(false);

		if (testAction.getActionResult() != null) {
			final GUITestActionResult actionResult = testAction.getActionResult();

			cboComponentType.getComboViewer().getCombo().setEnabled(true);
			cboComponentType.setSelectedItem(actionResult.getComponentType());

			txtResultMessage.setEnabled(true);
			txtResultMessage.setText(actionResult.getMessageText() != null ? actionResult.getMessageText() : "");

			cboStatus.getComboViewer().getCombo().setEnabled(true);
			cboStatus.setSelectedItem(actionResult.getStatus());

			chkResult.setSelection(true);
			lblMessage.setEnabled(true);
			lblStatus.setEnabled(true);
			lblOpen.setEnabled(true);
		}

		return panDialogArea;
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

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			GUITestActionResult actionResult = null;

			if (chkResult.getSelection()) {
				// Add the action result
				actionResult = TestingFactory.eINSTANCE.createGUITestActionResult();
				actionResult.setComponentType(cboComponentType.getSelectedItem());
				actionResult.setMessageText(txtResultMessage.getText());
				actionResult.setStatus(cboStatus.getSelectedItem());
				actionResult.setTestAction(testAction);
			}
			else
				testAction.setActionResult(null);
		}

		super.buttonPressed(buttonId);
	}

}
