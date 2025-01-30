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
package net.codecadenza.eclipse.ui.dialog.integration;

import java.util.Optional;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining integration method data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditIntegrationMethodDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit integration method";

	private final AbstractIntegrationMethod integrationMethod;
	private final IntegrationTechnology technology;
	private final Project project;
	private Text txtComment;
	private Text txtName;
	private Button chkStartNewThread;
	private Text txtOperationName;
	private Text txtReturnValueName;
	private Text txtReturnValuePartName;
	private Button chkAddParameterAnnotations;
	private Text txtPath;
	private Combo cboInputType;
	private Combo cboOutputType;
	private Combo cboHTTPMethod;
	private Text txtRequestSchema;
	private Text txtResponseSchema;
	private Button chkDedicatedPartition;
	private Button chkSendResponse;
	private Text txtOperationID;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param integrationMethod
	 */
	public EditIntegrationMethodDialog(Shell parentShell, AbstractIntegrationMethod integrationMethod) {
		super(parentShell);

		this.technology = integrationMethod.getIntegrationBean().getIntegrationTechnology();
		this.integrationMethod = integrationMethod;
		this.project = integrationMethod.getIntegrationBean().getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtName.setText(integrationMethod.getName());

		if (technology == IntegrationTechnology.SOAP) {
			final var soapMethod = (SOAPIntegrationMethod) integrationMethod;

			final var lblOperationName = new Label(panDialogArea, SWT.NONE);
			lblOperationName.setText("Operation name:");

			txtOperationName = new Text(panDialogArea, SWT.BORDER);
			txtOperationName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (soapMethod.getOperationName() != null)
				txtOperationName.setText(soapMethod.getOperationName());

			final var lblReturnValueName = new Label(panDialogArea, SWT.NONE);
			lblReturnValueName.setText("Return value name:");

			txtReturnValueName = new Text(panDialogArea, SWT.BORDER);
			txtReturnValueName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (soapMethod.getReturnValueName() != null)
				txtReturnValueName.setText(soapMethod.getReturnValueName());

			final var lblReturnValuePartName = new Label(panDialogArea, SWT.NONE);
			lblReturnValuePartName.setText("Return value part name:");

			txtReturnValuePartName = new Text(panDialogArea, SWT.BORDER);
			txtReturnValuePartName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (soapMethod.getReturnValuePartName() != null)
				txtReturnValuePartName.setText(soapMethod.getReturnValuePartName());

			final var lblAddParamAnnotations = new Label(panDialogArea, SWT.NONE);
			lblAddParamAnnotations.setText("Add parameter annotations:");

			chkAddParameterAnnotations = new Button(panDialogArea, SWT.CHECK);
			chkAddParameterAnnotations.setSelection(soapMethod.isAddParameterAnnotations());
		}
		else if (technology == IntegrationTechnology.REST) {
			final var restMethod = (RESTIntegrationMethod) integrationMethod;

			final var lblPath = new Label(panDialogArea, SWT.NONE);
			lblPath.setText("Path:");

			txtPath = new Text(panDialogArea, SWT.BORDER);
			txtPath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (restMethod.getPath() != null)
				txtPath.setText(restMethod.getPath());

			final var lblHTTPMethod = new Label(panDialogArea, SWT.NONE);
			lblHTTPMethod.setText("HTTP method:");

			cboHTTPMethod = new Combo(panDialogArea, SWT.READ_ONLY);
			cboHTTPMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (restMethod.getHttpMethod() == HttpMethodEnumeration.POST || restMethod.getHttpMethod() == HttpMethodEnumeration.PUT) {
				cboHTTPMethod.add(HttpMethodEnumeration.POST.getName());
				cboHTTPMethod.add(HttpMethodEnumeration.PUT.getName());
			}
			else {
				// It makes no sense to change the original HTTP method from GET to DELETE or vice versa!
				cboHTTPMethod.add(restMethod.getHttpMethod().getName());
			}

			cboHTTPMethod.select(cboHTTPMethod.indexOf(restMethod.getHttpMethod().getName()));

			final var lblInputType = new Label(panDialogArea, SWT.NONE);
			lblInputType.setText("Input content type:");

			if (restMethod.getInputType() == MediaTypeEnumeration.JSON || restMethod.getInputType() == MediaTypeEnumeration.XML) {
				cboInputType = new Combo(panDialogArea, SWT.READ_ONLY);
				cboInputType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				cboInputType.add(MediaTypeEnumeration.JSON.getName());
				cboInputType.add(MediaTypeEnumeration.XML.getName());
				cboInputType.select(cboInputType.indexOf(restMethod.getInputType().getName()));

				if (project.hasAngularClient())
					cboInputType.setEnabled(false);
			}
			else {
				final var txtInputType = new Text(panDialogArea, SWT.BORDER);
				txtInputType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				txtInputType.setText(restMethod.getInputType().getName());
				txtInputType.setEditable(false);
			}

			final var lblOutputType = new Label(panDialogArea, SWT.NONE);
			lblOutputType.setText("Output content type:");

			if (restMethod.getOutputType() == MediaTypeEnumeration.JSON || restMethod.getOutputType() == MediaTypeEnumeration.XML) {
				cboOutputType = new Combo(panDialogArea, SWT.READ_ONLY);
				cboOutputType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				cboOutputType.add(MediaTypeEnumeration.JSON.getName());
				cboOutputType.add(MediaTypeEnumeration.XML.getName());
				cboOutputType.select(cboOutputType.indexOf(restMethod.getOutputType().getName()));

				if (project.hasAngularClient())
					cboOutputType.setEnabled(false);
			}
			else {
				final var txtOutputType = new Text(panDialogArea, SWT.BORDER);
				txtOutputType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				txtOutputType.setText(restMethod.getOutputType().getName());
				txtOutputType.setEditable(false);
			}
		}
		else if (technology == IntegrationTechnology.KAFKA) {
			final var kafkaMethod = (KafkaIntegrationMethod) integrationMethod;

			final var lblRequestSchema = new Label(panDialogArea, SWT.NONE);
			lblRequestSchema.setText("Request schema name:");

			txtRequestSchema = new Text(panDialogArea, SWT.BORDER);
			txtRequestSchema.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtRequestSchema.setText(kafkaMethod.getRequestSchemaName());

			final var lblSendResponse = new Label(panDialogArea, SWT.NONE);
			lblSendResponse.setText("Send response:");

			chkSendResponse = new Button(panDialogArea, SWT.CHECK);
			chkSendResponse.setSelection(kafkaMethod.isSendResponse());

			chkSendResponse.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (!chkSendResponse.getSelection()) {
						txtResponseSchema.setText("");
						chkDedicatedPartition.setSelection(false);
					}

					txtResponseSchema.setEnabled(chkSendResponse.getSelection());
					chkDedicatedPartition.setEnabled(chkSendResponse.getSelection());
				}
			});

			final var lblResponseSchema = new Label(panDialogArea, SWT.NONE);
			lblResponseSchema.setText("Response schema name:");

			txtResponseSchema = new Text(panDialogArea, SWT.BORDER);
			txtResponseSchema.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtResponseSchema.setText(kafkaMethod.getResponseSchemaName());
			txtResponseSchema.setEnabled(kafkaMethod.isSendResponse());

			final var lblDedicatedPartition = new Label(panDialogArea, SWT.NONE);
			lblDedicatedPartition.setText("Use dedicated partition:");

			chkDedicatedPartition = new Button(panDialogArea, SWT.CHECK);
			chkDedicatedPartition.setSelection(kafkaMethod.isUseDedicatedPartition());
			chkDedicatedPartition.setEnabled(kafkaMethod.isSendResponse());
		}
		else if (technology == IntegrationTechnology.JMS) {
			final var jmsMethod = (JMSIntegrationMethod) integrationMethod;

			final var lblOperationID = new Label(panDialogArea, SWT.NONE);
			lblOperationID.setText("Operation ID:");

			txtOperationID = new Text(panDialogArea, SWT.BORDER);
			txtOperationID.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtOperationID.setText(jmsMethod.getOperationID());

			final var lblSendResponse = new Label(panDialogArea, SWT.NONE);
			lblSendResponse.setText("Send response:");

			chkSendResponse = new Button(panDialogArea, SWT.CHECK);
			chkSendResponse.setSelection(jmsMethod.isSendResponse());
		}

		if (technology != IntegrationTechnology.KAFKA && technology != IntegrationTechnology.JMS) {
			final var lblStartNewThread = new Label(panDialogArea, SWT.NONE);
			lblStartNewThread.setText("Start new thread:");

			chkStartNewThread = new Button(panDialogArea, SWT.CHECK);
			chkStartNewThread.setSelection(integrationMethod.isStartNewThread());
		}

		final var lblComment = new Label(panDialogArea, SWT.NONE);
		lblComment.setText("Comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdComment.heightHint = 100;
		gdComment.widthHint = 400;

		txtComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(gdComment);

		if (integrationMethod.getComment() != null)
			txtComment.setText(integrationMethod.getComment());

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
		if (buttonId == IDialogConstants.OK_ID && !saveData())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Validate the input and save changes
	 * @return true if the validation was successful
	 */
	private boolean saveData() {
		if (!validateJavaMethodName(txtName))
			return false;

		if (technology == IntegrationTechnology.SOAP) {
			if (!txtOperationName.getText().isEmpty() && !validateJavaMethodName(txtOperationName))
				return false;

			if (!txtReturnValueName.getText().isEmpty() && !validateTypeVariableName(txtReturnValueName))
				return false;

			if (!txtReturnValuePartName.getText().isEmpty() && !validateTypeVariableName(txtReturnValuePartName))
				return false;

			final var soapMethod = (SOAPIntegrationMethod) integrationMethod;
			soapMethod.setOperationName(txtOperationName.getText());
			soapMethod.setReturnValueName(txtReturnValueName.getText());
			soapMethod.setReturnValuePartName(txtReturnValuePartName.getText());
			soapMethod.setAddParameterAnnotations(chkAddParameterAnnotations.getSelection());
		}
		else if (technology == IntegrationTechnology.REST) {
			final var restMethod = (RESTIntegrationMethod) integrationMethod;
			restMethod.setPath(txtPath.getText());
			restMethod.setHttpMethod(HttpMethodEnumeration.valueOf(cboHTTPMethod.getItem(cboHTTPMethod.getSelectionIndex())));

			if (cboInputType != null)
				restMethod.setInputType(MediaTypeEnumeration.valueOf(cboInputType.getItem(cboInputType.getSelectionIndex())));

			if (cboOutputType != null)
				restMethod.setOutputType(MediaTypeEnumeration.valueOf(cboOutputType.getItem(cboOutputType.getSelectionIndex())));
		}
		else if (technology == IntegrationTechnology.KAFKA) {
			if (!validateJavaTypeName(txtRequestSchema))
				return false;

			if (!txtResponseSchema.getText().isEmpty()) {
				if (!validateJavaTypeName(txtResponseSchema))
					return false;

				if (txtRequestSchema.getText().equals(txtResponseSchema.getText())) {
					final var validationErrorMsg = "The names of the request and the response schema must be different!";

					MessageDialog.openInformation(getShell(), DLG_TITLE, validationErrorMsg);
					return false;
				}
			}

			final var kafkaMethod = (KafkaIntegrationMethod) integrationMethod;
			kafkaMethod.setRequestSchemaName(txtRequestSchema.getText());
			kafkaMethod.setResponseSchemaName(txtResponseSchema.getText());
			kafkaMethod.setUseDedicatedPartition(chkDedicatedPartition.getSelection());
			kafkaMethod.setSendResponse(chkSendResponse.getSelection());
		}
		else if (technology == IntegrationTechnology.JMS) {
			String validationErrorMsg = null;

			if (txtOperationID.getText().isEmpty())
				validationErrorMsg = "The operation ID must not be empty!";

			// Search for duplicate operation IDs
			final Optional<JMSIntegrationMethod> methodWithSameOperationID = integrationMethod.getIntegrationBean().getMethods()
					.stream().map(JMSIntegrationMethod.class::cast).filter(m -> !m.equals(integrationMethod))
					.filter(method -> method.getOperationID().equals(txtOperationID.getText())).findFirst();

			if (methodWithSameOperationID.isPresent())
				validationErrorMsg = "Duplicate operation ID found in method " + methodWithSameOperationID.get().getName() + "()!";

			if (validationErrorMsg != null) {
				txtOperationID.setFocus();
				MessageDialog.openInformation(getShell(), DLG_TITLE, validationErrorMsg);
				return false;
			}

			final var jmsMethod = (JMSIntegrationMethod) integrationMethod;
			jmsMethod.setOperationID(txtOperationID.getText());
			jmsMethod.setSendResponse(chkSendResponse.getSelection());
		}

		integrationMethod.setComment(txtComment.getText());
		integrationMethod.setName(txtName.getText());

		if (chkStartNewThread != null)
			integrationMethod.setStartNewThread(chkStartNewThread.getSelection());

		return true;
	}

	/**
	 * Check if the given text field contains a valid Java method name
	 * @param txtField
	 * @return false if the validation has failed
	 */
	private boolean validateJavaMethodName(Text txtField) {
		return evaluateStatus(EclipseIDEService.validateMethodName(txtField.getText()), txtField);
	}

	/**
	 * Check if the given text field contains a valid Java type name
	 * @param txtField
	 * @return false if the validation has failed
	 */
	private boolean validateJavaTypeName(Text txtField) {
		return evaluateStatus(EclipseIDEService.validateJavaTypeName(txtField.getText()), txtField);
	}

	/**
	 * Check if the given text field contains a valid Java type variable name
	 * @param txtField
	 * @return false if the validation has failed
	 */
	private boolean validateTypeVariableName(Text txtField) {
		return evaluateStatus(EclipseIDEService.validateTypeVariableName(txtField.getText()), txtField);
	}

	/**
	 * Evaluate the provided status and open a dialog if the status indicates either a validation error or a warning
	 * @param status
	 * @param txtField
	 * @return false if the provided status indicates either a validation error or a warning
	 */
	private boolean evaluateStatus(IStatus status, Text txtField) {
		if (status.getSeverity() > IStatus.INFO) {
			txtField.setFocus();
			MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
			return false;
		}

		return true;
	}

}
