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
package net.codecadenza.eclipse.ui.dialog.exchange;

import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_MODE_DIRECT;
import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_MODE_FILE;
import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_MODE_STRING;
import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_PATH_PARAM;
import static net.codecadenza.eclipse.shared.Constants.INVOCATION_MODE_ASYNC;
import static net.codecadenza.eclipse.shared.Constants.INVOCATION_MODE_DEFAULT;
import static net.codecadenza.eclipse.shared.Constants.INVOCATION_MODE_SCHEDULED;
import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.nio.charset.Charset;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.ScheduledInvocation;
import net.codecadenza.eclipse.model.service.ServiceFactory;
import net.codecadenza.eclipse.service.exchange.DataExchangeBeanService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import net.codecadenza.eclipse.ui.panel.EditDataExchangeElementPanel;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining data exchange methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditExchangeMethodDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE_EDIT = "Edit data exchange method";
	private static final String DLG_TITLE_NEW = "Create new data exchange method";
	private static final String DEFAULT_DATE_TIME_FORMAT = "dd.MM.yyyy HH:mm:ss";
	private static final String DEFAULT_DATE_FORMAT = "dd.MM.yyyy";
	private static final String DEFAULT_NUMBER_FORMAT = "0.00";
	private static final String SCHEMA_FILE_SUFFIX = ".xsd";

	private final Project project;
	private final DataExchangeBeanService dataExchangeBeanService;
	private DataExchangeMethodTypeEnumeration selectedMethodType;
	private ContentTypeEnumeration selectedContentType;
	private String selectedExchangeMode;
	private DomainObject selectedDomainObject;
	private Text txtDomainObject;
	private DomainObjectProposalTextField propDomainObject;
	private Text txtMethodName;
	private Text txtCharset;
	private Text txtMaxNoOfObjects;
	private Text txtPath;
	private Text txtFileNamePattern;
	private Text txtBlockSize;
	private Text txtInvocationSecond;
	private Text txtInvocationMinute;
	private Text txtInvocationHour;
	private Text txtInvocationDayOfWeek;
	private Text txtInvocationDayOfMonth;
	private Text txtInvocationMonth;
	private Text txtInvocationYear;
	private Text txtInvocationDelay;
	private Button chkFormatOutput;
	private Label lblFormatOutput;
	private boolean editMode;
	private DataExchangeMethod dataExchangeMethod;
	private Combo cboExchangeMode;
	private Text txtGeneratedStatement;
	private Text txtCustomStatement;
	private Label lblGenStatement;
	private Label lblCustomStatement;
	private Combo cboPermMode;
	private CheckboxDataGridComposite<Role> chkViewerRoles;
	private Label lblRoles;
	private Label lblInvocationSecond;
	private Label lblInvocationMinute;
	private Label lblInvocationHour;
	private Label lblInvocationDayOfWeek;
	private Label lblInvocationDayOfMonth;
	private Label lblInvocationMonth;
	private Label lblInvocationYear;
	private Label lblInvocationDelay;
	private Button chkAddToBoundary;
	private Combo cboInvocationMode;
	private Button chkValidation;
	private Button chkProcessSingleObject;
	private Composite panInvocationSettings;
	private Label lblPath;
	private Label lblFileNamePattern;
	private Label lblBlockSize;
	private Composite panExchangeMode;
	private Label lblMaximumNoOf;
	private Button chkTransactionPerFile;
	private Label lblTransactionPerFile;
	private Label lblDeleteAfterImport;
	private Button chkDeleteAfterImport;
	private Label lblTargetPathAfterImport;
	private Text txtTargetPathAfterImport;
	private DataComboViewer<DataExchangeMethod> cboJoinedMethod;
	private Text txtSchemaFileName;
	private Combo cboMethodType;
	private Combo cboContentType;
	private DataGridComposite<FilterMethodParameter> gridParameters;
	private Label lblQuoteCharacter;
	private Label lblDelimiter;
	private Label lblRecordSeparator;
	private Label lblCommentCharacter;
	private Label lblDefaultDateFormat;
	private Label lblDefaultDateTimeFormat;
	private Label lblDefaultNumberFormat;
	private Text txtDelimiter;
	private Text txtRecordSeparator;
	private Text txtCommentCharacter;
	private Text txtDefaultDateFormat;
	private Text txtDefaultDateTimeFormat;
	private Text txtDefaultNumberFormat;
	private Text txtQuoteCharacter;
	private Button chkAlternativePathHandling;
	private Label lblAlternativePathHandling;
	private Text txtComment;
	private EditDataExchangeElementPanel panDataExchangeElement;
	private boolean referencedByBoundaryMethod;

	/**
	 * Constructor
	 * @param parentShell
	 * @param project
	 */
	public EditExchangeMethodDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
		this.dataExchangeBeanService = new DataExchangeBeanService(project);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param dataExchangeMethod
	 */
	public EditExchangeMethodDialog(Shell parentShell, DataExchangeMethod dataExchangeMethod) {
		super(parentShell);

		this.editMode = true;
		this.dataExchangeMethod = dataExchangeMethod;
		this.project = dataExchangeMethod.getDataExchangeServiceBean().getNamespace().getProject();
		this.selectedDomainObject = dataExchangeMethod.getDataExchangeServiceBean().getDomainObject();
		this.dataExchangeBeanService = new DataExchangeBeanService(project);

		// Check if this data exchange method is directly referenced by a boundary method
		final BoundaryBean boundary = project.getBoundaryByDomainObject(selectedDomainObject);

		if (boundary != null)
			this.referencedByBoundaryMethod = boundary.getBoundaryMethods().stream()
					.anyMatch(e -> e.getServiceMethod().equals(dataExchangeMethod));
	}

	/**
	 * @return the default method name
	 */
	private String buildDefaultMethodName() {
		var methodName = "";

		if (selectedDomainObject == null)
			return methodName;

		if (!selectedExchangeMode.equals(EXCHANGE_MODE_DIRECT)) {
			methodName = "perform" + selectedDomainObject.getName();

			if (selectedContentType == ContentTypeEnumeration.EXCEL2007 || selectedContentType == ContentTypeEnumeration.EXCEL97)
				methodName += "Excel";
			else
				methodName += selectedContentType.getName();

			if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT)
				methodName += "Export";
			else
				methodName += "Import";
		}
		else {
			if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT)
				methodName += "export";
			else
				methodName += "import";

			if (chkProcessSingleObject.getSelection())
				methodName += selectedDomainObject.getUpperCaseName();
			else
				methodName += selectedDomainObject.getNamePlural().substring(0, 1).toUpperCase()
						+ selectedDomainObject.getNamePlural().substring(1);
		}

		return methodName;
	}

	/**
	 * @return the default comment for a data exchange method
	 */
	private String buildDefaultComment() {
		var comment = "";

		if (selectedDomainObject == null)
			return comment;

		if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT)
			comment += "Export ";
		else
			comment += "Import ";

		comment += selectedDomainObject.getLabel();

		if (chkProcessSingleObject.getSelection())
			comment += " object ";
		else
			comment += " objects ";

		if (!selectedExchangeMode.equals(EXCHANGE_MODE_DIRECT)) {
			if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT)
				comment += "to ";
			else
				comment += "from ";

			if (selectedContentType == ContentTypeEnumeration.EXCEL2007 || selectedContentType == ContentTypeEnumeration.EXCEL97)
				comment += "Microsoft Excel ";
			else
				comment += selectedContentType.getName() + " ";

			if (selectedExchangeMode.equals(EXCHANGE_MODE_FILE))
				comment += "file";
			else
				comment += "content string";
		}

		return comment;
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateInput() {
		setErrorMessage(null);

		if (!editMode && propDomainObject.getSelectedItem() == null) {
			setErrorMessage("A domain object must be selected!");
			propDomainObject.getControl().setFocus();
			return false;
		}

		final IStatus status = EclipseIDEService.validateMethodName(txtMethodName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			txtMethodName.setFocus();
			return false;
		}

		final String invocationMode = cboInvocationMode.getItem(cboInvocationMode.getSelectionIndex());

		if (invocationMode.equals(INVOCATION_MODE_ASYNC) && !txtInvocationDelay.getText().isEmpty())
			try {
				final int delay = Integer.parseInt(txtInvocationDelay.getText());

				// The value must not be negative!
				if (delay < 0)
					throw new NumberFormatException();
			}
			catch (final NumberFormatException e) {
				setErrorMessage("The invocation delay requires a positive integer value!");
				txtInvocationDelay.setFocus();
				return false;
			}

		if (txtMaxNoOfObjects != null && !txtMaxNoOfObjects.getText().isEmpty()) {
			try {
				Integer.parseInt(txtMaxNoOfObjects.getText());
			}
			catch (final NumberFormatException e) {
				setErrorMessage("The maximum no. of objects requires an integer value!");
				txtMaxNoOfObjects.setFocus();
				return false;
			}
		}

		if (selectedExchangeMode.equals(EXCHANGE_MODE_FILE)) {
			if (txtPath.getText().isEmpty() && (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT
					|| (selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT && !chkAlternativePathHandling.getSelection()))) {
				setErrorMessage("The path must not be empty!");
				txtPath.setFocus();
				return false;
			}

			if (txtFileNamePattern.getText().isEmpty() && selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT) {
				setErrorMessage("The file name pattern must not be empty!");
				txtFileNamePattern.setFocus();
				return false;
			}

			if (txtBlockSize != null && !txtBlockSize.getText().isEmpty()) {
				try {
					Integer.parseInt(txtBlockSize.getText());
				}
				catch (final NumberFormatException e) {
					setErrorMessage("The file block size requires an integer value!");
					txtBlockSize.setFocus();
					return false;
				}
			}
		}

		if (!txtCharset.getText().isEmpty() && !Charset.availableCharsets().containsKey(txtCharset.getText())) {
			setErrorMessage("The selected character set is not supported!");
			txtCharset.setFocus();
			return false;
		}

		if (selectedContentType == ContentTypeEnumeration.CSV) {
			if (txtDelimiter.getText().isEmpty()) {
				setErrorMessage("The delimiter must not be empty!");
				txtDelimiter.setFocus();
				return false;
			}

			if (txtDelimiter.getText().length() > 1) {
				setErrorMessage("The delimiter must not have more than one character!");
				txtDelimiter.setFocus();
				return false;
			}

			if (txtQuoteCharacter.getText().length() > 1) {
				setErrorMessage("The quote character must not have more than one character!");
				txtQuoteCharacter.setFocus();
				return false;
			}

			if (txtCommentCharacter.getText().length() > 1) {
				setErrorMessage("The comment character must not have more than one character!");
				txtCommentCharacter.setFocus();
				return false;
			}

			if (txtRecordSeparator.getText().isEmpty()) {
				setErrorMessage("The record separator must not be empty!");
				txtRecordSeparator.setFocus();
				return false;
			}

			if (txtDefaultDateFormat.getText().isEmpty()) {
				setErrorMessage("The default date format must not be empty!");
				txtDefaultDateFormat.setFocus();
				return false;
			}

			try {
				new SimpleDateFormat().applyPattern(txtDefaultDateFormat.getText());
			}
			catch (final Exception e) {
				setErrorMessage("The default date format is not valid!");
				txtDefaultDateFormat.setFocus();
				return false;
			}

			if (txtDefaultDateTimeFormat.getText().isEmpty()) {
				setErrorMessage("The default date time format must not be empty!");
				txtDefaultDateTimeFormat.setFocus();
				return false;
			}

			try {
				new SimpleDateFormat().applyPattern(txtDefaultDateTimeFormat.getText());
			}
			catch (final Exception e) {
				setErrorMessage("The default date time format is not valid!");
				txtDefaultDateTimeFormat.setFocus();
				return false;
			}

			if (txtDefaultNumberFormat.getText().isEmpty()) {
				setErrorMessage("The default number format must not be empty!");
				txtDefaultNumberFormat.setFocus();
				return false;
			}

			try {
				new DecimalFormat().applyPattern(txtDefaultNumberFormat.getText());
			}
			catch (final Exception e) {
				setErrorMessage("The default number format is not valid!");
				txtDefaultNumberFormat.setFocus();
				return false;
			}
		}

		return true;
	}

	/**
	 * @return the generated default query statement
	 */
	private String generateDefaultQueryStatement() {
		final var b = new StringBuilder();

		if (selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT || chkProcessSingleObject.getSelection())
			return b.toString();

		b.append("select a from " + selectedDomainObject.getName() + " a");

		boolean firstParam = true;

		for (final FilterMethodParameter filterParam : gridParameters.getData()) {
			b.append(" ");

			if (firstParam) {
				b.append("where");
				firstParam = false;
			}
			else
				b.append("and");

			b.append(" a.");

			if (filterParam.getAssociation() != null)
				b.append(filterParam.getAssociation().getName() + ".");

			b.append(filterParam.getDomainAttribute().getName() + " ");
			b.append(filterParam.getOperator() + " :");
			b.append(filterParam.getName());
		}

		return b.toString();
	}

	/**
	 * Save the method
	 * @throws Exception if the save operation has failed
	 */
	private void saveMethod() throws Exception {
		if (!editMode) {
			dataExchangeMethod = ExchangeFactory.eINSTANCE.createDataExchangeMethod();
			dataExchangeMethod.setReturnType(project.getJavaTypeByName(JavaType.VOID));
		}

		dataExchangeMethod.setName(txtMethodName.getText());
		dataExchangeMethod.setCharset(txtCharset.getText());
		dataExchangeMethod.setFormatOutput(chkFormatOutput.getSelection());
		dataExchangeMethod.setPerformValidation(chkValidation.getSelection());
		dataExchangeMethod.setPermissionMode(PermissionModeEnumeration.valueOf(cboPermMode.getItem(cboPermMode.getSelectionIndex())));
		dataExchangeMethod.setQueryStatement(txtGeneratedStatement.getText());
		dataExchangeMethod.setCustomStatement(txtCustomStatement.getText());
		dataExchangeMethod.setRecordSeparator(txtRecordSeparator.getText());
		dataExchangeMethod.setSchemaFileName(txtSchemaFileName.getText());
		dataExchangeMethod.setComment(txtComment.getText());

		if (!txtDelimiter.getText().isEmpty())
			dataExchangeMethod.setDelimiter(txtDelimiter.getText().charAt(0));
		else
			dataExchangeMethod.setDelimiter(Character.MIN_VALUE);

		if (!txtCommentCharacter.getText().isEmpty())
			dataExchangeMethod.setCommentCharacter(txtCommentCharacter.getText().charAt(0));
		else
			dataExchangeMethod.setCommentCharacter(Character.MIN_VALUE);

		if (!txtQuoteCharacter.getText().isEmpty())
			dataExchangeMethod.setQuoteCharacter(txtQuoteCharacter.getText().charAt(0));
		else
			dataExchangeMethod.setQuoteCharacter(Character.MIN_VALUE);

		dataExchangeMethod.setDefaultDateFormat(txtDefaultDateFormat.getText());
		dataExchangeMethod.setDefaultDateTimeFormat(txtDefaultDateTimeFormat.getText());
		dataExchangeMethod.setDefaultNumberFormat(txtDefaultNumberFormat.getText());

		if (!editMode) {
			dataExchangeMethod.setMethodType(selectedMethodType);
			dataExchangeMethod.setContentType(selectedContentType);
			dataExchangeMethod.setProcessSingleObject(chkProcessSingleObject.getSelection());
			dataExchangeMethod.getAssociationControllers().addAll(panDataExchangeElement.getAssociationControllers());

			if (cboJoinedMethod.getSelectedItem() != null && !cboJoinedMethod.getSelectedItem().getName().isEmpty())
				dataExchangeMethod.setJoinedImportMethod(cboJoinedMethod.getSelectedItem());
			else
				dataExchangeMethod.setRootElement(panDataExchangeElement.getDataExchangeElement());

			gridParameters.getData().forEach(param -> {
				dataExchangeMethod.getMethodParameters().add(param);
				param.setMethod(dataExchangeMethod);
			});
		}

		if (selectedExchangeMode.equals(EXCHANGE_MODE_STRING)) {
			final StringExchangeMode exchangeMode = ExchangeFactory.eINSTANCE.createStringExchangeMode();

			if (txtMaxNoOfObjects != null && !txtMaxNoOfObjects.getText().isEmpty())
				exchangeMode.setMaxObjectsToBeProcessed(Integer.parseInt(txtMaxNoOfObjects.getText()));

			dataExchangeMethod.setExchangeMode(exchangeMode);
		}
		else if (selectedExchangeMode.equals(EXCHANGE_MODE_FILE)) {
			final FileExchangeMode exchangeMode = ExchangeFactory.eINSTANCE.createFileExchangeMode();

			if (txtMaxNoOfObjects != null && !txtMaxNoOfObjects.getText().isEmpty())
				exchangeMode.setMaxObjectsToBeProcessed(Integer.parseInt(txtMaxNoOfObjects.getText()));

			if (chkTransactionPerFile != null)
				exchangeMode.setNewTransactionPerFile(chkTransactionPerFile.getSelection());

			if (selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT) {
				if (chkDeleteAfterImport != null)
					exchangeMode.setDeleteAfterImport(chkDeleteAfterImport.getSelection());

				if (txtTargetPathAfterImport != null)
					exchangeMode.setTargetPathAfterImport(txtTargetPathAfterImport.getText());

				if (chkAlternativePathHandling.getSelection() && !editMode) {
					final MethodParameter pathParam = JavaFactory.eINSTANCE.createMethodParameter();
					pathParam.setName(EXCHANGE_PATH_PARAM);
					pathParam.setType(project.getJavaTypeByName(JavaType.STRING));
					pathParam.setMethod(dataExchangeMethod);

					dataExchangeMethod.getMethodParameters().add(pathParam);
				}

				if (!chkAlternativePathHandling.getSelection()) {
					exchangeMode.setFileNamePattern(txtFileNamePattern.getText());
					exchangeMode.setPath(txtPath.getText());
				}
			}
			else {
				exchangeMode.setFileNamePattern(txtFileNamePattern.getText());
				exchangeMode.setPath(txtPath.getText());

				if (chkAlternativePathHandling.getSelection())
					dataExchangeMethod.setReturnType(project.getJavaTypeByName(JavaType.STRING));
				else if (txtBlockSize != null && !txtBlockSize.getText().isEmpty())
					exchangeMode.setBlockSize(Integer.parseInt(txtBlockSize.getText()));
			}

			dataExchangeMethod.setExchangeMode(exchangeMode);
		}
		else if (selectedExchangeMode.equals(EXCHANGE_MODE_DIRECT)) {
			final DirectExchangeMode exchangeMode = ExchangeFactory.eINSTANCE.createDirectExchangeMode();

			if (txtMaxNoOfObjects != null && !txtMaxNoOfObjects.getText().isEmpty())
				exchangeMode.setMaxObjectsToBeProcessed(Integer.parseInt(txtMaxNoOfObjects.getText()));

			dataExchangeMethod.setExchangeMode(exchangeMode);
		}

		final String invocationMode = cboInvocationMode.getItem(cboInvocationMode.getSelectionIndex());

		if (invocationMode.equals(INVOCATION_MODE_SCHEDULED)) {
			final ScheduledInvocation invocation = ServiceFactory.eINSTANCE.createScheduledInvocation();
			invocation.setDayOfMonth(txtInvocationDayOfMonth.getText());
			invocation.setDayOfWeek(txtInvocationDayOfWeek.getText());
			invocation.setHour(txtInvocationHour.getText());
			invocation.setMinute(txtInvocationMinute.getText());
			invocation.setMonth(txtInvocationMonth.getText());
			invocation.setSecond(txtInvocationSecond.getText());
			invocation.setYear(txtInvocationYear.getText());
			invocation.setMethod(dataExchangeMethod);

			dataExchangeMethod.setMethodInvocation(invocation);
		}
		else if (invocationMode.equals(INVOCATION_MODE_ASYNC)) {
			final AsynchronousInvocation invocation = ServiceFactory.eINSTANCE.createAsynchronousInvocation();

			if (!txtInvocationDelay.getText().isEmpty())
				invocation.setDelayInMilliseconds(Integer.parseInt(txtInvocationDelay.getText()));

			invocation.setMethod(dataExchangeMethod);
			dataExchangeMethod.setMethodInvocation(invocation);
		}
		else
			dataExchangeMethod.setMethodInvocation(null);

		if (PermissionModeEnumeration
				.valueOf(cboPermMode.getItem(cboPermMode.getSelectionIndex())) == PermissionModeEnumeration.DEDICATED_ROLES) {
			dataExchangeMethod.getRoles().clear();

			// Grant all selected roles to this method
			dataExchangeMethod.getRoles().addAll(chkViewerRoles.getCheckedElements());
		}
		else
			dataExchangeMethod.getRoles().clear();

		final boolean addToBoundary = chkAddToBoundary != null && chkAddToBoundary.getSelection();

		if (!editMode)
			dataExchangeBeanService.createNewDataExchangeMethod(dataExchangeMethod, selectedDomainObject, addToBoundary);
		else
			dataExchangeBeanService.updateDataExchangeMethod(dataExchangeMethod, addToBoundary);
	}

	/**
	 * Add valid invocation modes to the respective field
	 */
	private void addValidInvocationModes() {
		boolean asyncModeEnabled = false;
		boolean scheduledModeEnabled = false;

		cboInvocationMode.add(INVOCATION_MODE_DEFAULT);

		if (project.isJakartaEEApplication() || project.isSpringBootApplication()) {
			cboInvocationMode.add(INVOCATION_MODE_ASYNC);
			cboInvocationMode.add(INVOCATION_MODE_SCHEDULED);

			asyncModeEnabled = true;
			scheduledModeEnabled = true;
		}

		if (editMode) {
			String currentMode = INVOCATION_MODE_DEFAULT;

			if (dataExchangeMethod.getMethodInvocation() != null) {
				if (scheduledModeEnabled && dataExchangeMethod.getMethodInvocation() instanceof ScheduledInvocation)
					currentMode = INVOCATION_MODE_SCHEDULED;
				else if (asyncModeEnabled && dataExchangeMethod.getMethodInvocation() instanceof AsynchronousInvocation)
					currentMode = INVOCATION_MODE_ASYNC;
			}

			if (cboInvocationMode.indexOf(currentMode) > 0)
				cboInvocationMode.select(cboInvocationMode.indexOf(currentMode));
			else
				cboInvocationMode.select(0);

			initializeInvocationFields(currentMode);
		}
		else
			cboInvocationMode.select(0);
	}

	/**
	 * @param mode
	 */
	private void initializeInvocationFields(String mode) {
		// Dispose dynamic form fields
		if (lblInvocationSecond != null)
			lblInvocationSecond.dispose();

		if (txtInvocationSecond != null)
			txtInvocationSecond.dispose();

		if (lblInvocationMinute != null)
			lblInvocationMinute.dispose();

		if (txtInvocationMinute != null)
			txtInvocationMinute.dispose();

		if (txtInvocationMinute != null)
			txtInvocationMinute.dispose();

		if (lblInvocationHour != null)
			lblInvocationHour.dispose();

		if (txtInvocationHour != null)
			txtInvocationHour.dispose();

		if (lblInvocationDayOfWeek != null)
			lblInvocationDayOfWeek.dispose();

		if (txtInvocationDayOfWeek != null)
			txtInvocationDayOfWeek.dispose();

		if (lblInvocationDayOfMonth != null)
			lblInvocationDayOfMonth.dispose();

		if (txtInvocationDayOfMonth != null)
			txtInvocationDayOfMonth.dispose();

		if (lblInvocationMonth != null)
			lblInvocationMonth.dispose();

		if (txtInvocationMonth != null)
			txtInvocationMonth.dispose();

		if (lblInvocationYear != null)
			lblInvocationYear.dispose();

		if (txtInvocationYear != null)
			txtInvocationYear.dispose();

		if (lblInvocationDelay != null)
			lblInvocationDelay.dispose();

		if (txtInvocationDelay != null)
			txtInvocationDelay.dispose();

		if (mode.equals(INVOCATION_MODE_SCHEDULED)) {
			lblInvocationSecond = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationSecond.setText("Second:");

			txtInvocationSecond = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationSecond.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblInvocationMinute = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationMinute.setText("Minute:");

			txtInvocationMinute = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationMinute.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblInvocationHour = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationHour.setText("Hour:");

			txtInvocationHour = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationHour.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblInvocationDayOfWeek = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationDayOfWeek.setText("Day of week:");

			txtInvocationDayOfWeek = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationDayOfWeek.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblInvocationDayOfMonth = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationDayOfMonth.setText("Day of month:");

			txtInvocationDayOfMonth = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationDayOfMonth.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblInvocationMonth = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationMonth.setText("Month:");

			txtInvocationMonth = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationMonth.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblInvocationYear = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationYear.setText("Year:");

			txtInvocationYear = new Text(panInvocationSettings, SWT.BORDER);
			txtInvocationYear.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (editMode && dataExchangeMethod.getMethodInvocation() instanceof final ScheduledInvocation invocation) {
				if (invocation.getSecond() != null)
					txtInvocationSecond.setText(invocation.getSecond());

				if (invocation.getMinute() != null)
					txtInvocationMinute.setText(invocation.getMinute());

				if (invocation.getHour() != null)
					txtInvocationHour.setText(invocation.getHour());

				if (invocation.getDayOfWeek() != null)
					txtInvocationDayOfWeek.setText(invocation.getDayOfWeek());

				if (invocation.getDayOfMonth() != null)
					txtInvocationDayOfMonth.setText(invocation.getDayOfMonth());

				if (invocation.getMonth() != null)
					txtInvocationMonth.setText(invocation.getMonth());

				if (invocation.getYear() != null)
					txtInvocationYear.setText(invocation.getYear());
			}
			else {
				// Initialize fields with default values
				txtInvocationSecond.setText("0");
				txtInvocationMinute.setText("0");
				txtInvocationHour.setText("0");
				txtInvocationDayOfWeek.setText("*");
				txtInvocationDayOfMonth.setText("*");
				txtInvocationMonth.setText("*");
				txtInvocationYear.setText("*");
			}
		}
		else if (mode.equals(INVOCATION_MODE_ASYNC)) {
			lblInvocationDelay = new Label(panInvocationSettings, SWT.NONE);
			lblInvocationDelay.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
			lblInvocationDelay.setText("Invocation delay (in ms): ");

			txtInvocationDelay = new Text(panInvocationSettings, SWT.BORDER);

			if (editMode && dataExchangeMethod.getMethodInvocation() instanceof final AsynchronousInvocation invocation
					&& invocation.getDelayInMilliseconds() != null)
				txtInvocationDelay.setText(invocation.getDelayInMilliseconds().toString());
		}

		panInvocationSettings.layout();
	}

	/**
	 * @return the generated default file name
	 */
	private String generateDefaultFileName() {
		return selectedDomainObject.getName() + "." + selectedContentType.getDefaultFileExtension();
	}

	/**
	 * Initialize all fields that depend on the selected data exchange mode
	 */
	private void initializeModeFields() {
		// Dispose dynamic form fields
		if (txtPath != null) {
			txtPath.dispose();
			txtPath = null;
		}

		if (lblPath != null) {
			lblPath.dispose();
			lblPath = null;
		}

		if (lblFileNamePattern != null) {
			lblFileNamePattern.dispose();
			lblFileNamePattern = null;
		}

		if (txtFileNamePattern != null) {
			txtFileNamePattern.dispose();
			txtFileNamePattern = null;
		}

		if (lblBlockSize != null) {
			lblBlockSize.dispose();
			lblBlockSize = null;
		}

		if (txtBlockSize != null) {
			txtBlockSize.dispose();
			txtBlockSize = null;
		}

		if (lblMaximumNoOf != null) {
			lblMaximumNoOf.dispose();
			lblMaximumNoOf = null;
		}

		if (txtMaxNoOfObjects != null) {
			txtMaxNoOfObjects.dispose();
			txtMaxNoOfObjects = null;
		}

		if (lblTransactionPerFile != null) {
			lblTransactionPerFile.dispose();
			lblTransactionPerFile = null;
		}

		if (chkTransactionPerFile != null) {
			chkTransactionPerFile.dispose();
			chkTransactionPerFile = null;
		}

		if (lblDeleteAfterImport != null) {
			lblDeleteAfterImport.dispose();
			lblDeleteAfterImport = null;
		}

		if (chkDeleteAfterImport != null) {
			chkDeleteAfterImport.dispose();
			chkDeleteAfterImport = null;
		}

		if (lblTargetPathAfterImport != null) {
			lblTargetPathAfterImport.dispose();
			lblTargetPathAfterImport = null;
		}

		if (txtTargetPathAfterImport != null) {
			txtTargetPathAfterImport.dispose();
			txtTargetPathAfterImport = null;
		}

		if (chkAlternativePathHandling != null) {
			chkAlternativePathHandling.dispose();
			chkAlternativePathHandling = null;
		}

		if (lblAlternativePathHandling != null) {
			lblAlternativePathHandling.dispose();
			lblAlternativePathHandling = null;
		}

		if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT && !chkProcessSingleObject.getSelection()) {
			lblMaximumNoOf = new Label(panExchangeMode, SWT.NONE);
			lblMaximumNoOf.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
			lblMaximumNoOf.setText("Maximum no. of objects to be processed:");

			txtMaxNoOfObjects = new Text(panExchangeMode, SWT.BORDER);
			txtMaxNoOfObjects.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
		}

		if (selectedExchangeMode.equals(EXCHANGE_MODE_FILE)) {
			if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT) {
				lblBlockSize = new Label(panExchangeMode, SWT.NONE);
				lblBlockSize.setText("File block size:");

				txtBlockSize = new Text(panExchangeMode, SWT.BORDER);
				txtBlockSize.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
			}

			lblPath = new Label(panExchangeMode, SWT.NONE);
			lblPath.setText("Path:");

			txtPath = new Text(panExchangeMode, SWT.BORDER);
			txtPath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			lblFileNamePattern = new Label(panExchangeMode, SWT.NONE);
			lblFileNamePattern.setText("File name pattern:");

			var fileNamePatternToolTip = "This field supports generation of dynamic file names. ";
			fileNamePatternToolTip += "The syntax is as follows: {element(value, format)}. ";
			fileNamePatternToolTip += "Supported elements are 'date(now, format)', 'date(domainObjectAttributeName, format)', ";
			fileNamePatternToolTip += "'batchindex(format)' and 'attr(domainObjectAttributeName, format)'. ";
			fileNamePatternToolTip += "Note that elements like 'batchindex' and 'attr' are not supported for import operations! ";
			fileNamePatternToolTip += "Elements that are using a domain attribute can be used in single-object mode only!";

			txtFileNamePattern = new Text(panExchangeMode, SWT.BORDER);
			txtFileNamePattern.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtFileNamePattern.setToolTipText(fileNamePatternToolTip);

			if (selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT) {
				lblTransactionPerFile = new Label(panExchangeMode, SWT.NONE);
				lblTransactionPerFile.setText("Start new transaction per file:");

				chkTransactionPerFile = new Button(panExchangeMode, SWT.CHECK);

				lblDeleteAfterImport = new Label(panExchangeMode, SWT.NONE);
				lblDeleteAfterImport.setText("Delete file after import:");

				chkDeleteAfterImport = new Button(panExchangeMode, SWT.CHECK);

				lblTargetPathAfterImport = new Label(panExchangeMode, SWT.NONE);
				lblTargetPathAfterImport.setText("Copy file after import to:");

				txtTargetPathAfterImport = new Text(panExchangeMode, SWT.BORDER);
				txtTargetPathAfterImport.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			}

			lblAlternativePathHandling = new Label(panExchangeMode, SWT.NONE);
			lblAlternativePathHandling.setText("Add parameter for path:");

			if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT)
				lblAlternativePathHandling.setText("Return path of generated file:");

			chkAlternativePathHandling = new Button(panExchangeMode, SWT.CHECK);

			if (editMode) {
				lblAlternativePathHandling.setEnabled(false);
				chkAlternativePathHandling.setEnabled(false);

				if (dataExchangeMethod.getExchangeMode() instanceof final FileExchangeMode fileExchangeMode) {
					if (fileExchangeMode.getFileNamePattern() != null)
						txtFileNamePattern.setText(fileExchangeMode.getFileNamePattern());

					if (fileExchangeMode.getPath() != null)
						txtPath.setText(fileExchangeMode.getPath());

					if (txtBlockSize != null && fileExchangeMode.getBlockSize() != null)
						txtBlockSize.setText(fileExchangeMode.getBlockSize().toString());

					if (chkTransactionPerFile != null)
						chkTransactionPerFile.setSelection(fileExchangeMode.isNewTransactionPerFile());

					if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT) {
						lblAlternativePathHandling.setText("Returns path of generated file:");

						if (dataExchangeMethod.returnsPath()) {
							txtBlockSize.setEnabled(false);
							txtBlockSize.setText("");
							lblBlockSize.setEnabled(false);
							chkAlternativePathHandling.setSelection(true);
						}
					}

					if (selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT) {
						lblAlternativePathHandling.setText("Has a path parameter:");
						chkDeleteAfterImport.setSelection(fileExchangeMode.isDeleteAfterImport());

						if (fileExchangeMode.getTargetPathAfterImport() != null)
							txtTargetPathAfterImport.setText(fileExchangeMode.getTargetPathAfterImport());

						if (dataExchangeMethod.hasPathParameter()) {
							txtPath.setEnabled(false);
							lblPath.setEnabled(false);
							txtFileNamePattern.setEnabled(false);
							lblFileNamePattern.setEnabled(false);
							chkAlternativePathHandling.setSelection(true);
						}
					}
				}
			}
			else {
				final var defaultPath = System.getProperty("user.home").replace("\\", "\\\\");

				txtPath.setText(defaultPath);
				txtFileNamePattern.setText(generateDefaultFileName());

				chkAlternativePathHandling.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						txtFileNamePattern.setText("");
						txtPath.setText("");

						if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT) {
							txtPath.setText(defaultPath);
							txtFileNamePattern.setText(generateDefaultFileName());

							if (chkAlternativePathHandling.getSelection()) {
								txtBlockSize.setEnabled(false);
								txtBlockSize.setText("");
								lblBlockSize.setEnabled(false);
							}
							else {
								txtBlockSize.setEnabled(true);
								lblBlockSize.setEnabled(true);
							}
						}
						else if (chkAlternativePathHandling.getSelection()) {
							txtFileNamePattern.setEnabled(false);
							lblFileNamePattern.setEnabled(false);
							txtPath.setEnabled(false);
							lblPath.setEnabled(false);
						}
						else {
							txtFileNamePattern.setEnabled(true);
							txtFileNamePattern.setText(generateDefaultFileName());
							txtPath.setEnabled(true);
							txtPath.setText(defaultPath);
							lblFileNamePattern.setEnabled(true);
							lblPath.setEnabled(true);
						}
					}
				});
			}
		}

		if (editMode && txtMaxNoOfObjects != null && dataExchangeMethod.getExchangeMode().getMaxObjectsToBeProcessed() != null)
			txtMaxNoOfObjects.setText(dataExchangeMethod.getExchangeMode().getMaxObjectsToBeProcessed().toString());

		panExchangeMode.layout();

		refreshDataExchangeElementPanel();
	}

	/**
	 * Refresh the data exchange element panel
	 */
	private void refreshDataExchangeElementPanel() {
		if (editMode) {
			panDataExchangeElement.setContentType(dataExchangeMethod.getContentType());
			panDataExchangeElement.setMethodType(dataExchangeMethod.getMethodType());
			panDataExchangeElement.setAssociationControllers(dataExchangeMethod.getAssociationControllers());
			panDataExchangeElement.setDateFormat(dataExchangeMethod.getDefaultDateFormat());
			panDataExchangeElement.setDateTimeFormat(dataExchangeMethod.getDefaultDateTimeFormat());
			panDataExchangeElement.setNumberFormat(dataExchangeMethod.getDefaultNumberFormat());
			panDataExchangeElement.setSingleObject(dataExchangeMethod.isProcessSingleObject());
			panDataExchangeElement.setJoined(dataExchangeMethod.getJoinedImportMethod() != null);
			panDataExchangeElement.setDomainObject(dataExchangeMethod.getDataExchangeServiceBean().getDomainObject());
			panDataExchangeElement.setEditMode(true);

			if (dataExchangeMethod.getJoinedImportMethod() != null)
				panDataExchangeElement.setDataExchangeElement(dataExchangeMethod.getJoinedImportMethod().getRootElement());
			else
				panDataExchangeElement.setDataExchangeElement(dataExchangeMethod.getRootElement());
		}
		else {
			panDataExchangeElement.setContentType(selectedContentType);
			panDataExchangeElement.setMethodType(selectedMethodType);
			panDataExchangeElement
					.setJoined(cboJoinedMethod.getSelectedItem() != null && !cboJoinedMethod.getSelectedItem().getName().isEmpty());
			panDataExchangeElement.setDomainObject(selectedDomainObject);
			panDataExchangeElement.setSingleObject(chkProcessSingleObject.getSelection());
			panDataExchangeElement.setDateFormat(txtDefaultDateFormat.getText());
			panDataExchangeElement.setDateTimeFormat(txtDefaultDateTimeFormat.getText());
			panDataExchangeElement.setNumberFormat(txtDefaultNumberFormat.getText());
			panDataExchangeElement.setEditMode(false);
		}

		panDataExchangeElement.refresh();
	}

	/**
	 * Add methods to the respective form field that can be joined to this method
	 */
	private void addValidJoinMethods() {
		final var methods = new BasicEList<DataExchangeMethod>();

		cboJoinedMethod.setEnabled(false);
		cboJoinedMethod.setData(methods);

		if (selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT)
			return;

		if (editMode) {
			if (dataExchangeMethod.getJoinedImportMethod() != null) {
				methods.add(dataExchangeMethod.getJoinedImportMethod());

				cboJoinedMethod.setData(methods);
				cboJoinedMethod.setSelectedItem(dataExchangeMethod.getJoinedImportMethod());
			}

			return;
		}

		// Add an empty method
		final DataExchangeMethod emptyMethod = ExchangeFactory.eINSTANCE.createDataExchangeMethod();
		emptyMethod.setName("");

		// Get all possible methods to be joined
		final DataExchangeServiceBean exchangeBean = dataExchangeBeanService
				.getDataExchangeServiceByDomainObject(selectedDomainObject);

		if (exchangeBean != null)
			for (final DataExchangeMethod m : exchangeBean.getDataExchangeMethods()) {
				// The content type must match! But it is allowed to mix different Microsoft Excel versions!
				if (m.getContentType() == ContentTypeEnumeration.EXCEL97 || m.getContentType() == ContentTypeEnumeration.EXCEL2007) {
					if (selectedContentType != ContentTypeEnumeration.EXCEL97 && selectedContentType != ContentTypeEnumeration.EXCEL2007)
						continue;
				}
				else if (m.getContentType() != selectedContentType)
					continue;

				// It must be an import method!
				if (m.getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT)
					continue;

				// Both methods must share the same data structures!
				if (m.isProcessSingleObject() != chkProcessSingleObject.getSelection())
					continue;

				methods.add(m);
			}

		methods.add(emptyMethod);

		cboJoinedMethod.setData(methods);
		cboJoinedMethod.setSelectedItem(emptyMethod);
		cboJoinedMethod.setEnabled(true);
	}

	/**
	 * Initialize the form
	 */
	private void initializeForm() {
		gridParameters.setEnabled(false);
		txtGeneratedStatement.setEnabled(false);
		txtCustomStatement.setEnabled(false);
		chkFormatOutput.setEnabled(false);
		chkFormatOutput.setSelection(false);
		lblFormatOutput.setEnabled(false);
		lblCustomStatement.setEnabled(false);
		lblGenStatement.setEnabled(false);
		txtDelimiter.setEnabled(false);
		txtRecordSeparator.setEnabled(false);
		txtCommentCharacter.setEnabled(false);
		txtQuoteCharacter.setEnabled(false);
		txtDefaultDateFormat.setEnabled(false);
		txtDefaultDateTimeFormat.setEnabled(false);
		txtDefaultNumberFormat.setEnabled(false);
		lblDelimiter.setEnabled(false);
		lblQuoteCharacter.setEnabled(false);
		lblCommentCharacter.setEnabled(false);
		lblRecordSeparator.setEnabled(false);
		lblDefaultDateFormat.setEnabled(false);
		lblDefaultDateTimeFormat.setEnabled(false);
		lblDefaultNumberFormat.setEnabled(false);
		txtSchemaFileName.setEnabled(false);
		txtSchemaFileName.setText("");

		if (editMode) {
			setTitle(DLG_TITLE_EDIT);
			setMessage("Change method data");
		}
		else {
			setTitle(DLG_TITLE_NEW);
			setMessage("Insert method data");
		}

		if (selectedDomainObject == null)
			return;

		if (getButton(OK) != null)
			getButton(OK).setEnabled(true);

		if (!editMode) {
			selectedMethodType = DataExchangeMethodTypeEnumeration.valueOf(cboMethodType.getItem(cboMethodType.getSelectionIndex()));
			selectedContentType = ContentTypeEnumeration.valueOf(cboContentType.getItem(cboContentType.getSelectionIndex()));

			if (selectedDomainObject.isAbstract() && selectedMethodType == DataExchangeMethodTypeEnumeration.IMPORT) {
				getButton(OK).setEnabled(false);

				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW,
						"A data import method cannot be created based on an abstract domain object!");
				return;
			}

			cboExchangeMode.removeAll();

			if (selectedContentType == ContentTypeEnumeration.XML || selectedContentType == ContentTypeEnumeration.JSON
					|| selectedContentType == ContentTypeEnumeration.CSV)
				cboExchangeMode.add(EXCHANGE_MODE_STRING);

			if (selectedContentType == ContentTypeEnumeration.XML)
				cboExchangeMode.add(EXCHANGE_MODE_DIRECT);

			cboExchangeMode.add(EXCHANGE_MODE_FILE);
			cboExchangeMode.select(0);

			chkFormatOutput.setSelection(selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT);
			chkValidation.setSelection(true);

			if (selectedContentType == ContentTypeEnumeration.CSV) {
				txtDelimiter.setText(";");
				txtRecordSeparator.setText("\\n");
				txtQuoteCharacter.setText("\"");
				txtCommentCharacter.setText("#");
				txtDefaultDateTimeFormat.setText(DEFAULT_DATE_TIME_FORMAT);
				txtDefaultDateFormat.setText(DEFAULT_DATE_FORMAT);
				txtDefaultNumberFormat.setText(DEFAULT_NUMBER_FORMAT);
			}
			else {
				txtDelimiter.setText("");
				txtRecordSeparator.setText("");
				txtQuoteCharacter.setText("");
				txtCommentCharacter.setText("");
				txtDefaultDateTimeFormat.setText("");
				txtDefaultDateFormat.setText("");
				txtDefaultNumberFormat.setText("");
			}

			selectedExchangeMode = cboExchangeMode.getItem(cboExchangeMode.getSelectionIndex());
			txtMethodName.setText(buildDefaultMethodName());
			txtComment.setText(buildDefaultComment());
			txtCharset.setText(UTF_8);

			int defaultIndex = 0;

			for (int i = 0; i < cboPermMode.getItemCount(); i++)
				if (cboPermMode.getItem(i).equals(PermissionModeEnumeration.PERMIT_ALL.name())) {
					defaultIndex = i;
					break;
				}

			cboPermMode.select(defaultIndex);
			gridParameters.setData(new BasicEList<>());
			txtGeneratedStatement.setText(generateDefaultQueryStatement());
			txtCustomStatement.setText("");
			propDomainObject.getControl().setFocus();
		}
		else {
			selectedMethodType = dataExchangeMethod.getMethodType();
			selectedContentType = dataExchangeMethod.getContentType();

			cboMethodType.add(dataExchangeMethod.getMethodType().getName());
			cboMethodType.select(0);

			cboContentType.add(dataExchangeMethod.getContentType().name());
			cboContentType.select(0);

			if (dataExchangeMethod.getExchangeMode() instanceof StringExchangeMode)
				selectedExchangeMode = EXCHANGE_MODE_STRING;
			else if (dataExchangeMethod.getExchangeMode() instanceof FileExchangeMode)
				selectedExchangeMode = EXCHANGE_MODE_FILE;
			else if (dataExchangeMethod.getExchangeMode() instanceof DirectExchangeMode)
				selectedExchangeMode = EXCHANGE_MODE_DIRECT;

			cboExchangeMode.add(selectedExchangeMode);
			cboExchangeMode.select(0);

			txtDomainObject.setText(dataExchangeMethod.getDataExchangeServiceBean().getDomainObject().getName());
			txtMethodName.setText(dataExchangeMethod.getName());
			chkFormatOutput.setSelection(dataExchangeMethod.isFormatOutput());
			chkValidation.setSelection(dataExchangeMethod.isPerformValidation());
			txtCharset.setText(dataExchangeMethod.getCharset());
			txtDelimiter.setText(Character.toString(dataExchangeMethod.getDelimiter()));
			txtRecordSeparator.setText(dataExchangeMethod.getRecordSeparator());

			if (dataExchangeMethod.getComment() != null)
				txtComment.setText(dataExchangeMethod.getComment());

			if (dataExchangeMethod.getQuoteCharacter() != Character.MIN_VALUE)
				txtQuoteCharacter.setText(Character.toString(dataExchangeMethod.getQuoteCharacter()));

			if (dataExchangeMethod.getCommentCharacter() != Character.MIN_VALUE)
				txtCommentCharacter.setText(Character.toString(dataExchangeMethod.getCommentCharacter()));

			txtDefaultDateFormat.setText(dataExchangeMethod.getDefaultDateFormat());
			txtDefaultNumberFormat.setText(dataExchangeMethod.getDefaultNumberFormat());
			txtDefaultDateTimeFormat.setText(dataExchangeMethod.getDefaultDateTimeFormat());

			chkProcessSingleObject.setSelection(dataExchangeMethod.isProcessSingleObject());
			chkProcessSingleObject.setEnabled(false);

			int i = 0;

			for (final String item : cboPermMode.getItems()) {
				if (item.equals(dataExchangeMethod.getPermissionMode().name())) {
					cboPermMode.select(i);
					break;
				}

				i++;
			}

			chkViewerRoles.setCheckedElements(dataExchangeMethod.getRoles());

			final var params = new BasicEList<FilterMethodParameter>();

			for (final MethodParameter param : dataExchangeMethod.getMethodParameters())
				if (param instanceof final FilterMethodParameter filterMethodParam)
					params.add(filterMethodParam);

			gridParameters.setData(params);

			txtGeneratedStatement.setText(dataExchangeMethod.getQueryStatement());
			txtCustomStatement.setText(dataExchangeMethod.getCustomStatement());

			txtDomainObject.setFocus();
		}

		if (PermissionModeEnumeration
				.valueOf(cboPermMode.getItem(cboPermMode.getSelectionIndex())) == PermissionModeEnumeration.DEDICATED_ROLES) {
			lblRoles.setEnabled(true);
			chkViewerRoles.getTableViewer().getTable().setEnabled(true);
		}

		if (selectedMethodType == DataExchangeMethodTypeEnumeration.EXPORT) {
			chkFormatOutput.setEnabled(true);
			lblFormatOutput.setEnabled(true);

			if (!chkProcessSingleObject.getSelection()) {
				gridParameters.setEnabled(true);
				txtGeneratedStatement.setEnabled(true);
				txtCustomStatement.setEnabled(true);
				lblCustomStatement.setEnabled(true);
				lblGenStatement.setEnabled(true);
			}
		}

		addValidJoinMethods();

		initializeModeFields();

		if (selectedContentType == ContentTypeEnumeration.CSV) {
			txtDelimiter.setEnabled(true);
			txtRecordSeparator.setEnabled(true);
			txtCommentCharacter.setEnabled(true);
			txtQuoteCharacter.setEnabled(true);
			txtDefaultDateFormat.setEnabled(true);
			txtDefaultDateTimeFormat.setEnabled(true);
			txtDefaultNumberFormat.setEnabled(true);
			lblDelimiter.setEnabled(true);
			lblQuoteCharacter.setEnabled(true);
			lblCommentCharacter.setEnabled(true);
			lblRecordSeparator.setEnabled(true);
			lblDefaultDateFormat.setEnabled(true);
			lblDefaultDateTimeFormat.setEnabled(true);
			lblDefaultNumberFormat.setEnabled(true);
		}
		else if (selectedContentType == ContentTypeEnumeration.XML) {
			final DataExchangeMethod selItem = cboJoinedMethod.getSelectedItem();

			if (selItem == null || selItem.getName().isEmpty()) {
				txtSchemaFileName.setEnabled(true);

				if (editMode && dataExchangeMethod.getSchemaFileName() != null)
					txtSchemaFileName.setText(dataExchangeMethod.getSchemaFileName());

				if (!editMode)
					txtSchemaFileName.setText(selectedDomainObject.getName() + SCHEMA_FILE_SUFFIX);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var tabFolderMain = new TabFolder(panDialogArea, SWT.NONE);
		tabFolderMain.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var tabItemBasicData = new TabItem(tabFolderMain, SWT.NONE);
		tabItemBasicData.setText("Basic data");

		final var panBasicData = new Composite(tabFolderMain, SWT.NONE);
		panBasicData.setLayout(new GridLayout(4, false));

		tabItemBasicData.setControl(panBasicData);

		final var lblDomainObject = new Label(panBasicData, SWT.NONE);
		lblDomainObject.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));

		if (!editMode) {
			lblDomainObject.setText("Select domain object:");

			propDomainObject = new DomainObjectProposalTextField(panBasicData, project) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang. Object)
				 */
				@Override
				public void onProposalAccepted(DomainObject domainObject) {
					try {
						// Setup the domain object tree
						selectedDomainObject = domainObject;

						initializeForm();
					}
					catch (final Exception e) {
						CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
					}
				}
			};

			propDomainObject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else {
			lblDomainObject.setText("Domain object:");
			txtDomainObject = new Text(panBasicData, SWT.BORDER);
			txtDomainObject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtDomainObject.setEditable(false);
		}

		final var lblNumber = new Label(panBasicData, SWT.NONE);
		lblNumber.setText("Process single object:");

		chkProcessSingleObject = new Button(panBasicData, SWT.CHECK);
		chkProcessSingleObject.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));

		if (!editMode)
			chkProcessSingleObject.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					initializeForm();
				}
			});

		final var lblMethodType = new Label(panBasicData, SWT.NONE);
		lblMethodType.setText("Method type:");

		cboMethodType = new Combo(panBasicData, SWT.NONE | SWT.READ_ONLY);
		cboMethodType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (!editMode)
			cboMethodType.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					initializeForm();
				}
			});

		final var lblContentType = new Label(panBasicData, SWT.NONE);
		lblContentType.setText("Content type:");

		cboContentType = new Combo(panBasicData, SWT.NONE | SWT.READ_ONLY);
		cboContentType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (!editMode)
			cboContentType.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					initializeForm();
				}
			});

		final var lblMethodName = new Label(panBasicData, SWT.NONE);
		lblMethodName.setText("Method name:");

		txtMethodName = new Text(panBasicData, SWT.BORDER);
		txtMethodName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblCharset = new Label(panBasicData, SWT.NONE);
		lblCharset.setText("Character set:");

		txtCharset = new Text(panBasicData, SWT.BORDER);
		txtCharset.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblPerformValidation = new Label(panBasicData, SWT.NONE);
		lblPerformValidation.setText("Perform validation:");

		chkValidation = new Button(panBasicData, SWT.CHECK);
		chkValidation.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));

		lblFormatOutput = new Label(panBasicData, SWT.NONE);
		lblFormatOutput.setText("Format output:");

		chkFormatOutput = new Button(panBasicData, SWT.CHECK);

		final var lblJoinedMethod = new Label(panBasicData, SWT.NONE);
		lblJoinedMethod.setText("Joined method:");

		cboJoinedMethod = new DataComboViewer<>(panBasicData, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(DataExchangeMethod element) {
				return element.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(DataExchangeMethod selectedMethod) {
				if (selectedMethod != null && !selectedMethod.getName().isEmpty() || selectedContentType != ContentTypeEnumeration.XML) {
					txtSchemaFileName.setEnabled(false);
					txtSchemaFileName.setText("");
				}
				else {
					txtSchemaFileName.setEnabled(true);
					txtSchemaFileName.setText(selectedDomainObject.getName() + SCHEMA_FILE_SUFFIX);
				}

				refreshDataExchangeElementPanel();
			}
		};

		cboJoinedMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblSchemaFileName = new Label(panBasicData, SWT.NONE);
		lblSchemaFileName.setText("Schema file:");

		txtSchemaFileName = new Text(panBasicData, SWT.BORDER);
		txtSchemaFileName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (!referencedByBoundaryMethod) {
			final var lblAddToBoundary = new Label(panBasicData, SWT.NONE);

			if (project.isBoundaryMode())
				lblAddToBoundary.setText("Add to boundary bean:");
			else
				lblAddToBoundary.setText("Add to facade bean:");

			chkAddToBoundary = new Button(panBasicData, SWT.CHECK);
			chkAddToBoundary.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
		}

		// Add some empty labels in order to create some extra space necessary for other panels
		new Label(panBasicData, SWT.NONE);
		new Label(panBasicData, SWT.NONE);

		new Label(panBasicData, SWT.NONE);
		new Label(panBasicData, SWT.NONE);
		new Label(panBasicData, SWT.NONE);
		new Label(panBasicData, SWT.NONE);

		// Add a tab folder that contains the format settings
		final var tabItemFormat = new TabItem(tabFolderMain, SWT.NONE);
		tabItemFormat.setText("Format settings");

		final var panFormat = new Composite(tabFolderMain, SWT.NONE);
		panFormat.setLayout(new GridLayout(4, false));

		tabItemFormat.setControl(panFormat);

		lblDelimiter = new Label(panFormat, SWT.NONE);
		lblDelimiter.setText("Delimiter:");

		txtDelimiter = new Text(panFormat, SWT.BORDER);
		txtDelimiter.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblQuoteCharacter = new Label(panFormat, SWT.NONE);
		lblQuoteCharacter.setText("Quote character:");

		txtQuoteCharacter = new Text(panFormat, SWT.BORDER);
		txtQuoteCharacter.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblCommentCharacter = new Label(panFormat, SWT.NONE);
		lblCommentCharacter.setText("Comment character:");

		txtCommentCharacter = new Text(panFormat, SWT.BORDER);
		txtCommentCharacter.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblRecordSeparator = new Label(panFormat, SWT.NONE);
		lblRecordSeparator.setText("Record separator:");

		txtRecordSeparator = new Text(panFormat, SWT.BORDER);
		txtRecordSeparator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblDefaultDateFormat = new Label(panFormat, SWT.NONE);
		lblDefaultDateFormat.setText("Default date format:");

		txtDefaultDateFormat = new Text(panFormat, SWT.BORDER);
		txtDefaultDateFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblDefaultDateTimeFormat = new Label(panFormat, SWT.NONE);
		lblDefaultDateTimeFormat.setText("Default date time format:");

		txtDefaultDateTimeFormat = new Text(panFormat, SWT.BORDER);
		txtDefaultDateTimeFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		lblDefaultNumberFormat = new Label(panFormat, SWT.NONE);
		lblDefaultNumberFormat.setText("Default number format:");

		txtDefaultNumberFormat = new Text(panFormat, SWT.BORDER);
		txtDefaultNumberFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var tabItemExchangeMode = new TabItem(tabFolderMain, SWT.NONE);
		tabItemExchangeMode.setText("Exchange mode");

		panExchangeMode = new Composite(tabFolderMain, SWT.NONE);
		panExchangeMode.setLayout(new GridLayout(2, false));

		tabItemExchangeMode.setControl(panExchangeMode);

		final var lblSelectMode = new Label(panExchangeMode, SWT.NONE);
		lblSelectMode.setText("Select mode:");

		cboExchangeMode = new Combo(panExchangeMode, SWT.NONE | SWT.READ_ONLY);
		cboExchangeMode.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));

		if (!editMode)
			cboExchangeMode.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					selectedExchangeMode = cboExchangeMode.getItem(cboExchangeMode.getSelectionIndex());

					// In case of DIRECT mode it makes sense to create a respective boundary method!
					if (chkAddToBoundary != null)
						chkAddToBoundary.setSelection(selectedExchangeMode.equals(EXCHANGE_MODE_DIRECT));

					txtMethodName.setText(buildDefaultMethodName());
					txtComment.setText(buildDefaultComment());

					initializeModeFields();
				}
			});

		final var tabItemSecurity = new TabItem(tabFolderMain, SWT.NONE);
		tabItemSecurity.setText("Security settings");

		final var panSecuritySettings = new Composite(tabFolderMain, SWT.NONE);
		panSecuritySettings.setLayout(new GridLayout(2, false));

		tabItemSecurity.setControl(panSecuritySettings);

		final var lblPermissionMode = new Label(panSecuritySettings, SWT.NONE);
		lblPermissionMode.setText("Permission mode:");

		cboPermMode = new Combo(panSecuritySettings, SWT.READ_ONLY);

		cboPermMode.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String mode = cboPermMode.getItem(cboPermMode.getSelectionIndex());

				if (PermissionModeEnumeration.valueOf(mode) == PermissionModeEnumeration.DEDICATED_ROLES) {
					lblRoles.setEnabled(true);
					chkViewerRoles.getTableViewer().getTable().setEnabled(true);
				}
				else {
					lblRoles.setEnabled(false);
					chkViewerRoles.getTableViewer().getTable().setEnabled(false);
					chkViewerRoles.uncheckAllElements();
				}
			}
		});

		cboPermMode.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));

		// Add permission modes
		for (final PermissionModeEnumeration permMode : PermissionModeEnumeration.values())
			cboPermMode.add(permMode.toString());

		lblRoles = new Label(panSecuritySettings, SWT.NONE);
		lblRoles.setText("Select roles:");
		lblRoles.setEnabled(false);

		chkViewerRoles = new CheckboxDataGridComposite<>(panSecuritySettings, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(Role element, int columnIndex) {
				if (columnIndex == 0)
					return element.getName();

				return "";
			}
		};

		chkViewerRoles.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		chkViewerRoles.addColumn("Name", ColumnSortType.STRING, 400);
		chkViewerRoles.getTableViewer().getTable().setLinesVisible(false);
		chkViewerRoles.setData(project.getRoles());
		chkViewerRoles.getTableViewer().getTable().setEnabled(false);

		final var tabItemInvocationSettings = new TabItem(tabFolderMain, SWT.NONE);
		tabItemInvocationSettings.setText("Invocation settings");

		panInvocationSettings = new Composite(tabFolderMain, SWT.NONE);
		panInvocationSettings.setLayout(new GridLayout(4, false));

		tabItemInvocationSettings.setControl(panInvocationSettings);

		final var lblInvocationMode = new Label(panInvocationSettings, SWT.NONE);
		lblInvocationMode.setText("Select mode:");

		cboInvocationMode = new Combo(panInvocationSettings, SWT.NONE | SWT.READ_ONLY);
		cboInvocationMode.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));

		cboInvocationMode.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String mode = cboInvocationMode.getItem(cboInvocationMode.getSelectionIndex());

				initializeInvocationFields(mode);
			}
		});

		new Label(panInvocationSettings, SWT.NONE);
		new Label(panInvocationSettings, SWT.NONE);

		final var gdTabFolderDetails = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdTabFolderDetails.heightHint = 400;

		final var tabFolderDetails = new TabFolder(panDialogArea, SWT.NONE);
		tabFolderDetails.setLayoutData(gdTabFolderDetails);

		final var tabItemMapping = new TabItem(tabFolderDetails, SWT.NONE);
		tabItemMapping.setText("Mapping");

		panDataExchangeElement = new EditDataExchangeElementPanel(tabFolderDetails, project);

		tabItemMapping.setControl(panDataExchangeElement);

		final var tabItemParam = new TabItem(tabFolderDetails, SWT.NONE);
		tabItemParam.setText("Filter parameters");

		final var panParams = new Composite(tabFolderDetails, SWT.NONE);
		panParams.setLayout(new GridLayout());

		tabItemParam.setControl(panParams);

		gridParameters = new DataGridComposite<>(panParams, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(FilterMethodParameter selectedParameter, int columnIndex) {
				if (columnIndex == 0)
					return selectedParameter.getName();

				if (columnIndex == 1)
					return selectedParameter.getOperator();

				if (columnIndex == 2 && selectedParameter.getAssociation() != null)
					return selectedParameter.getAssociation().getName();

				if (columnIndex == 3)
					return selectedParameter.getDomainAttribute().getName();

				return "";
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(FilterMethodParameter selectedParameter) {
				if (selectedParameter == null)
					return;

				final var dlg = new EditFilterMethodParamDialog(getShell(), selectedDomainObject, selectedParameter);
				dlg.open();

				if (dlg.getReturnCode() == Dialog.OK) {
					gridParameters.refresh();
					txtGeneratedStatement.setText(generateDefaultQueryStatement());
				}
			}
		};

		gridParameters.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		gridParameters.addColumn("Parameter name", ColumnSortType.STRING, 200);
		gridParameters.addColumn("Operator", ColumnSortType.STRING, 150);
		gridParameters.addColumn("Domain association", ColumnSortType.STRING, 200);
		gridParameters.addColumn("Domain attribute", ColumnSortType.STRING, 150);

		// The signature of a data exchange method must not be changed if it is directly referenced by a boundary method!
		if (!referencedByBoundaryMethod) {
			final var mniAddParam = new MenuItem(gridParameters.getPopUpMenu(), SWT.NONE);
			mniAddParam.setText("Create new parameter");

			mniAddParam.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (selectedDomainObject == null)
						return;

					final var dlg = new EditFilterMethodParamDialog(getShell(), selectedDomainObject);
					dlg.open();

					if (dlg.getReturnCode() == Dialog.OK) {
						final FilterMethodParameter param = dlg.getFilterParameter();

						if (editMode) {
							param.setMethod(dataExchangeMethod);
							dataExchangeMethod.getMethodParameters().add(param);
						}

						gridParameters.getData().add(param);
						gridParameters.refresh();

						txtGeneratedStatement.setText(generateDefaultQueryStatement());
					}
				}
			});
		}

		final var mniEditParam = new MenuItem(gridParameters.getPopUpMenu(), SWT.NONE);
		mniEditParam.setText("Edit parameter");

		mniEditParam.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final FilterMethodParameter param = gridParameters.getSelection();

				if (param == null)
					return;

				final var dlg = new EditFilterMethodParamDialog(getShell(), selectedDomainObject, param);
				dlg.open();

				if (dlg.getReturnCode() == Dialog.OK) {
					gridParameters.refresh();
					txtGeneratedStatement.setText(generateDefaultQueryStatement());
				}
			}
		});

		if (!referencedByBoundaryMethod) {
			final var mniDeleteParam = new MenuItem(gridParameters.getPopUpMenu(), SWT.NONE);
			mniDeleteParam.setText("Delete parameter");

			mniDeleteParam.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final FilterMethodParameter param = gridParameters.getSelection();
					final var msgTitle = "Delete filter parameter";

					if (param == null)
						return;

					final boolean doIt = MessageDialog.openConfirm(getShell(), msgTitle,
							"Do you really want to delete the selected filter parameter?");

					if (!doIt)
						return;

					if (editMode)
						dataExchangeMethod.getMethodParameters().remove(param);

					gridParameters.getData().remove(param);
					gridParameters.refresh();

					txtGeneratedStatement.setText(generateDefaultQueryStatement());
				}
			});
		}

		final var tabItemQuery = new TabItem(tabFolderDetails, SWT.NONE);
		tabItemQuery.setText("Query statements");

		final var panStatements = new Composite(tabFolderDetails, SWT.NONE);
		panStatements.setLayout(new GridLayout());

		tabItemQuery.setControl(panStatements);

		lblGenStatement = new Label(panStatements, SWT.NONE);
		lblGenStatement.setText("Generated select statement:");
		lblGenStatement.setEnabled(false);

		txtGeneratedStatement = new Text(panStatements, SWT.BORDER | SWT.V_SCROLL | SWT.MULTI | SWT.WRAP);
		txtGeneratedStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
		txtGeneratedStatement.setEnabled(false);
		txtGeneratedStatement.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		lblCustomStatement = new Label(panStatements, SWT.NONE);
		lblCustomStatement.setText("Additional statement:");
		lblCustomStatement.setEnabled(false);

		txtCustomStatement = new Text(panStatements, SWT.BORDER | SWT.V_SCROLL | SWT.MULTI | SWT.WRAP);
		txtCustomStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
		txtCustomStatement.setEnabled(false);
		txtCustomStatement.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemComment = new TabItem(tabFolderMain, SWT.NONE);
		tabItemComment.setText("Comment");

		final var panComment = new Composite(tabFolderMain, SWT.NONE);
		panComment.setLayout(new GridLayout(2, false));

		tabItemComment.setControl(panComment);

		final var lblComment = new Label(panComment, SWT.NONE);
		lblComment.setText("Method comment:");

		txtComment = new Text(panComment, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!editMode) {
			for (final DataExchangeMethodTypeEnumeration type : DataExchangeMethodTypeEnumeration.values())
				cboMethodType.add(type.name());

			cboMethodType.select(0);

			for (final ContentTypeEnumeration type : ContentTypeEnumeration.values())
				cboContentType.add(type.name());

			cboContentType.select(0);
		}

		addValidInvocationModes();

		initializeForm();

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!validateInput())
				return;

			try {
				saveMethod();
			}
			catch (final Exception e) {
				setErrorMessage(e.getMessage());
				CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				return;
			}
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

		if (!editMode)
			newShell.setText(DLG_TITLE_NEW);
		else
			newShell.setText(DLG_TITLE_EDIT);
	}

}
