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
package net.codecadenza.eclipse.ui.dialog.testing.integration;

import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.util.Collection;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.util.db.SQLStatementSplitter;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.IntegrationTestInvocationPanel;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.InvocationTreePanel;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.NestedIntegrationTestInvocationPanel;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.TestInvocationPanel;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining integration test cases
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditIntegrationTestCaseDialog extends CodeCadenzaTitleAreaDialog {
	private final IntegrationTestModule testModule;
	private final IntegrationTestCase testCase;
	private final String defaultName;
	private final String title;
	private boolean editMode;
	private Text txtName;
	private AbstractProposalTextField<AbstractIntegrationBean> txtIntegrationBean;
	private DataComboViewer<AbstractIntegrationMethod> cboMethods;
	private Text txtComment;
	private Text txtUserName;
	private Text txtPassword;
	private InvocationTreePanel treeInvocations;
	private Group groupInvocation;
	private SashForm sashForm;
	private Composite panButtons;
	private Text txtPreStatements;
	private Text txtPostStatements;
	private TestInvocationPanel panActualInvocation;

	/**
	 * Create the dialog for creating a new integration test case
	 * @param parentShell
	 * @param testModule
	 */
	public EditIntegrationTestCaseDialog(Shell parentShell, IntegrationTestModule testModule) {
		super(parentShell);

		this.testModule = testModule;
		this.testCase = TestingFactory.eINSTANCE.createIntegrationTestCase();
		this.defaultName = "New" + testModule.getTestCaseSuffix();
		this.title = "Create new integration test case";
	}

	/**
	 * Create the dialog for updating an existing integration test case
	 * @param parentShell
	 * @param testCase
	 */
	public EditIntegrationTestCaseDialog(Shell parentShell, IntegrationTestCase testCase) {
		super(parentShell);

		this.testCase = testCase;
		this.testModule = (IntegrationTestModule) testCase.getTestModule();
		this.editMode = true;
		this.defaultName = "New" + testModule.getTestCaseSuffix();
		this.title = "Edit integration test case " + testCase.getName();
	}

	/**
	 * @return the integration test case
	 */
	public IntegrationTestCase getTestCase() {
		return testCase;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite panDialogArea = (Composite) super.createDialogArea(parent);
		panDialogArea.setLayout(new GridLayout());

		initBasicPanel(panDialogArea);
		initFields();

		setTitle(title);
		setMessage("Enter data...");

		if (editMode)
			cboMethods.setFocus();
		else
			txtIntegrationBean.setFocus();

		return panDialogArea;
	}

	/**
	 * Initialize the basic panel
	 * @param panDialogArea
	 */
	private void initBasicPanel(final Composite panDialogArea) {
		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayout(new GridLayout(4, false));
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblName = new Label(groupBasicData, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(groupBasicData, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		new Label(groupBasicData, SWT.NONE);
		new Label(groupBasicData, SWT.NONE);

		final var lblUserName = new Label(groupBasicData, SWT.NONE);
		lblUserName.setText("User name:");

		txtUserName = new Text(groupBasicData, SWT.BORDER);
		txtUserName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblPassword = new Label(groupBasicData, SWT.NONE);
		lblPassword.setText("Password:");

		txtPassword = new Text(groupBasicData, SWT.BORDER);
		txtPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblComment = new Label(groupBasicData, SWT.NONE);
		lblComment.setText("Comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1);
		gdComment.heightHint = 50;

		txtComment = new Text(groupBasicData, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(gdComment);

		final var tabFolder = new TabFolder(panDialogArea, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemInvocations = new TabItem(tabFolder, SWT.NONE);
		tabItemInvocations.setText("Method invocations");

		final var panInvocations = new Composite(tabFolder, SWT.NONE);
		panInvocations.setLayout(new GridLayout());
		panInvocations.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var groupAddNew = new Group(panInvocations, SWT.NONE);
		groupAddNew.setText("Add new invocation");
		groupAddNew.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		groupAddNew.setLayout(new GridLayout(5, false));

		final var lblIntegrationBean = new Label(groupAddNew, SWT.NONE);
		lblIntegrationBean.setText("Integration bean:");

		txtIntegrationBean = new AbstractProposalTextField<>(groupAddNew, SWT.BORDER, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang. Object)
			 */
			@Override
			public String getProposalLabel(AbstractIntegrationBean integrationBean) {
				return integrationBean.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<AbstractIntegrationBean> getProposalData(String filter) {
				final IntegrationModule integrationModule = testModule.getIntegrationModule();
				final Stream<JavaType> moduleTypes = integrationModule.getNamespace().getJavaTypes().stream();

				return moduleTypes.map(AbstractIntegrationBean.class::cast)
						.filter(a -> a.getName().toLowerCase().contains(filter.toLowerCase())).toList();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang. Object)
			 */
			@Override
			public void onProposalAccepted(AbstractIntegrationBean integrationBean) {
				cboMethods.setData(integrationBean.getMethods().stream().toList());
			}
		};

		txtIntegrationBean.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblMethods = new Label(groupAddNew, SWT.NONE);
		lblMethods.setText("Integration method:");

		cboMethods = new DataComboViewer<>(groupAddNew, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(AbstractIntegrationMethod method) {
				return method.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(AbstractIntegrationMethod method) {
				if (!editMode) {
					if (txtName.getText().equals(defaultName))
						txtName.setText(method.getIntegrationBean().getDomainObject().getName() + testModule.getTestCaseSuffix());

					if (txtComment.getText().isEmpty())
						txtComment.setText("Integration test for " + method.getIntegrationBean().getDomainObject().getLabel() + " objects");
				}
			}
		};

		cboMethods.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var panAddButton = new Composite(groupAddNew, SWT.NONE);
		panAddButton.setLayout(new GridLayout());
		panAddButton.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));

		final var gdAdd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		gdAdd.heightHint = 40;
		gdAdd.widthHint = 60;

		final var cmdAdd = new Button(panAddButton, SWT.PUSH);
		cmdAdd.setText("Add");
		cmdAdd.setLayoutData(gdAdd);

		cmdAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final AbstractIntegrationMethod selectedMethod = cboMethods.getSelectedItem();

				if (selectedMethod != null)
					createNewInvocation(selectedMethod);
			}
		});

		initTreePanel(panInvocations);

		tabItemInvocations.setControl(panInvocations);

		final var tabItemStatements = new TabItem(tabFolder, SWT.NONE);
		tabItemStatements.setText("Statements");

		tabItemStatements.setControl(initStatementsPanel(tabFolder));
	}

	/**
	 * Initialize the tree panel
	 * @param panParent
	 */
	private void initTreePanel(final Composite panParent) {
		final var gdSashForm = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdSashForm.heightHint = 600;
		gdSashForm.widthHint = 900;

		sashForm = new SashForm(panParent, SWT.NONE);
		sashForm.setLayoutData(gdSashForm);

		treeInvocations = new InvocationTreePanel(sashForm, testCase) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.component.InvocationTreePanel#
			 * onCreateNewNestedInvocation(net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation)
			 */
			@Override
			protected void onCreateNewNestedInvocation(IntegrationMethodTestInvocation parentInvocation) {
				createNewNestedInvocation(parentInvocation);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.component.InvocationTreePanel#
			 * onEditInvocation(net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation)
			 */
			@Override
			protected void onEditInvocation(IntegrationMethodTestInvocation methodInvocation) {
				editInvocation(methodInvocation);
			}
		};

		treeInvocations.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		groupInvocation = new Group(sashForm, SWT.NONE);
		groupInvocation.setText("No invocation selected");
		groupInvocation.setLayout(new GridLayout());

		sashForm.setWeights(30, 70);
	}

	/**
	 * Initialize the panel for the pre-processing and the post-processing statements
	 * @param panParent
	 * @return the new panel
	 */
	private Composite initStatementsPanel(Composite panParent) {
		final var sashStatements = new SashForm(panParent, SWT.VERTICAL);
		sashStatements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panPreStatements = new Composite(sashStatements, SWT.NONE);
		panPreStatements.setLayout(new GridLayout());
		panPreStatements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var lblPreStatements = new Label(panPreStatements, SWT.NONE);
		lblPreStatements.setText("Pre-processing statements:");

		txtPreStatements = new Text(panPreStatements, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.CANCEL | SWT.MULTI);
		txtPreStatements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		txtPreStatements.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
		txtPreStatements.setToolTipText("Statements must be delimited by a ';' character");

		final var panPostStatements = new Composite(sashStatements, SWT.NONE);
		panPostStatements.setLayout(new GridLayout());
		panPostStatements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var lblPostStatements = new Label(panPostStatements, SWT.NONE);
		lblPostStatements.setText("Post-processing statements:");
		lblPostStatements.setToolTipText("Statements must be delimited by a ';' character");

		txtPostStatements = new Text(panPostStatements, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.CANCEL | SWT.MULTI);
		txtPostStatements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		txtPostStatements.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		return sashStatements;
	}

	/**
	 * Create and add the components for creating a new nested invocation
	 * @param parentInvocation
	 */
	private void createNewNestedInvocation(final IntegrationMethodTestInvocation parentInvocation) {
		final var message = "Create new nested invocation for " + parentInvocation.getTestMethodName() + "()";

		if (!saveAndDisposeInvocation(true))
			return;

		setMessage(message);

		groupInvocation.setText(message);

		panActualInvocation = new NestedIntegrationTestInvocationPanel(groupInvocation, testModule, testCase, parentInvocation);
		panActualInvocation.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeInvocations.setEnableDelete(false);

		initInvocationButtonBar();

		sashForm.layout(true, true);
	}

	/**
	 * Create and add the components for creating a new invocation
	 * @param integrationMethod
	 */
	private void createNewInvocation(final AbstractIntegrationMethod integrationMethod) {
		final var message = "Create new invocation for " + integrationMethod.getName() + "()";

		if (!saveAndDisposeInvocation(true))
			return;

		setMessage(message);

		groupInvocation.setText(message);

		panActualInvocation = new IntegrationTestInvocationPanel(groupInvocation, testModule, testCase, integrationMethod);
		panActualInvocation.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeInvocations.setEnableDelete(false);

		initInvocationButtonBar();

		sashForm.layout(true, true);
	}

	/**
	 * Create and add the components for editing an existing invocation
	 * @param methodInvocation
	 */
	private void editInvocation(final IntegrationMethodTestInvocation methodInvocation) {
		if (!saveAndDisposeInvocation(true))
			return;

		if (methodInvocation.getParentInvocation() == null) {
			final var message = "Edit invocation " + methodInvocation.getTestMethodName() + "()";

			setMessage(message);

			groupInvocation.setText(message);
			panActualInvocation = new IntegrationTestInvocationPanel(groupInvocation, testModule, testCase, methodInvocation);
		}
		else {
			final var message = "Edit nested invocation " + methodInvocation.getTestMethodName() + "()";

			setMessage(message);

			groupInvocation.setText(message);
			panActualInvocation = new NestedIntegrationTestInvocationPanel(groupInvocation, testModule, testCase,
					methodInvocation.getParentInvocation(), methodInvocation);
		}

		treeInvocations.setEnableDelete(false);

		panActualInvocation.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		initInvocationButtonBar();

		sashForm.layout(true, true);
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
		if (buttonId == IDialogConstants.OK_ID) {
			if (!checkInput())
				return;

			try {
				saveTestCase();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * Initialize the button bar
	 */
	private void initInvocationButtonBar() {
		panButtons = new Composite(groupInvocation, SWT.NONE);
		panButtons.setLayout(new GridLayout(3, false));
		panButtons.setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM, true, false));

		final var gdButton = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		gdButton.widthHint = 90;
		gdButton.heightHint = 30;

		final var cmdSave = new Button(panButtons, SWT.PUSH);
		cmdSave.setLayoutData(gdButton);
		cmdSave.setText("Save");

		cmdSave.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				saveAndDisposeInvocation(true);
			}
		});

		final var cmdRandomValues = new Button(panButtons, SWT.PUSH);
		cmdRandomValues.setLayoutData(gdButton);
		cmdRandomValues.setText("Random");

		cmdRandomValues.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				panActualInvocation.generateRandomValues();

				setErrorMessage(null);
				setMessage("Random values generated");
			}
		});

		final var cmdCancel = new Button(panButtons, SWT.PUSH);
		cmdCancel.setLayoutData(gdButton);
		cmdCancel.setText("Cancel");

		cmdCancel.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				saveAndDisposeInvocation(false);
				setMessage("Operation cancelled");
			}
		});
	}

	/**
	 * Save the invocation and dispose the invocation panel and the button bar
	 * @param save controls if the active invocation should be saved
	 * @return true if the active invocation was saved successfully or if the no save operation was requested
	 */
	private boolean saveAndDisposeInvocation(boolean save) {
		setErrorMessage(null);

		if (panActualInvocation != null && save) {
			final String errorMessage = panActualInvocation.validateAndApplyInput();

			if (errorMessage != null) {
				setErrorMessage(errorMessage);
				return false;
			}
			else
				setMessage("Invocation saved successfully");
		}

		treeInvocations.refreshTree();

		if (panActualInvocation != null) {
			panActualInvocation.dispose();
			panActualInvocation = null;
		}

		if (panButtons != null)
			panButtons.dispose();

		groupInvocation.setText("No invocation selected");
		treeInvocations.setEnableDelete(true);

		sashForm.layout(true, true);

		return true;
	}

	/**
	 * Initialize the respective dialog components with the data of the test case
	 */
	private void initFields() {
		if (!editMode) {
			txtName.setText(defaultName);
			return;
		}

		if (!testCase.getMethodInvocations().isEmpty()) {
			final IntegrationMethodTestInvocation lastInvocation = testCase.getMethodInvocations().getLast();
			final AbstractIntegrationBean integrationBean = lastInvocation.getIntegrationMethod().getIntegrationBean();

			txtIntegrationBean.setSelectedItem(integrationBean);
			cboMethods.setData(integrationBean.getMethods().stream().toList());
		}

		txtName.setText(testCase.getName());
		txtComment.setText(testCase.getComment());
		txtUserName.setText(testCase.getUserName());
		txtPassword.setText(testCase.getPassword());

		if (testCase.getPreProcessingStatements() != null)
			txtPreStatements.setText(testCase.getPreProcessingStatements());

		if (testCase.getPostProcessingStatements() != null)
			txtPostStatements.setText(testCase.getPostProcessingStatements());

		treeInvocations.refreshTree();
	}

	/**
	 * Save the test case
	 * @throws Exception if the save operation has failed
	 */
	private void saveTestCase() throws Exception {
		final IntegrationTestCaseService integrationTestCaseService = new IntegrationTestCaseService(testModule);

		if (editMode) {
			// Check if the test case must be renamed
			if (!testCase.getName().equals(txtName.getText()))
				integrationTestCaseService.renameTestCase(testCase, txtName.getText());
		}
		else
			testCase.setNamespace(testModule.getNamespace());

		testCase.setName(txtName.getText());
		testCase.setComment(txtComment.getText());
		testCase.setUserName(txtUserName.getText());
		testCase.setPassword(txtPassword.getText());
		testCase.setPreProcessingStatements(txtPreStatements.getText());
		testCase.setPostProcessingStatements(txtPostStatements.getText());

		final Resource eResource = testModule.getNamespace().eResource();

		if (!eResource.getContents().contains(testCase))
			eResource.getContents().add(testCase);

		EclipseIDEService.saveProjectMetaData(testModule.getProject());

		// Rebuild the test case
		integrationTestCaseService.rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean checkInput() {
		final IStatus status = EclipseIDEService.validateJavaTypeName(txtName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			txtName.setFocus();
			return false;
		}

		try {
			if (!txtPreStatements.getText().isEmpty())
				SQLStatementSplitter.splitSQLStatements(txtPreStatements.getText());

			if (!txtPostStatements.getText().isEmpty())
				SQLStatementSplitter.splitSQLStatements(txtPostStatements.getText());
		}
		catch (final Exception e) {
			setErrorMessage(e.getMessage());
			return false;
		}

		// Check if another object with the same name already exists
		for (final JavaType type : testModule.getNamespace().getJavaTypes()) {
			if (editMode && type.equals(testCase) && testCase.getName().equals(txtName.getText()))
				continue;

			if (type.getName().equals(txtName.getText())) {
				setErrorMessage("A class with the same name already exists in this package!");
				txtName.setFocus();
				return false;
			}
		}

		setErrorMessage(null);
		return true;
	}

}
