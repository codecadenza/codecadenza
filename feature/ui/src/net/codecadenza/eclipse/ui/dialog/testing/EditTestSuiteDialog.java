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
package net.codecadenza.eclipse.ui.dialog.testing;

import java.util.List;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.service.testing.TestSuiteService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.DualDataSelectionListComposite;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining test suites
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditTestSuiteDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE_CREATE = "Create new test suite";
	private static final String DLG_TITLE_EDIT = "Edit test suite";

	private final Project project;
	private final AbstractTestModule testModule;
	private final TestSuite testSuite;
	private Text txtName;
	private Text txtComment;
	private DualDataSelectionListComposite<GUITestCase> listTestCases;
	private boolean editMode;
	private String title = DLG_TITLE_CREATE;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testSuite
	 */
	public EditTestSuiteDialog(Shell parentShell, TestSuite testSuite) {
		super(parentShell);

		this.testSuite = testSuite;
		this.testModule = testSuite.getTestModule();
		this.project = testSuite.getNamespace().getProject();
		this.title = DLG_TITLE_EDIT;
		this.editMode = true;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testModule
	 */
	public EditTestSuiteDialog(Shell parentShell, AbstractTestModule testModule) {
		super(parentShell);

		this.testModule = testModule;
		this.testSuite = TestingFactory.eINSTANCE.createTestSuite();
		this.project = testModule.getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		groupBasicData.setLayout(new GridLayout(2, false));

		final var lblName = new Label(groupBasicData, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(groupBasicData, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblComment = new Label(groupBasicData, SWT.NONE);
		lblComment.setText("Comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdComment.heightHint = 50;

		txtComment = new Text(groupBasicData, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(gdComment);

		final var gdTestCases = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdTestCases.heightHint = 300;

		final var panTestCases = new Group(panDialogArea, SWT.NONE);
		panTestCases.setLayout(new GridLayout());
		panTestCases.setLayoutData(gdTestCases);
		panTestCases.setText("Select test cases that should be included");

		listTestCases = new DualDataSelectionListComposite<>(panTestCases, SWT.NONE, false) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DualDataSelectionListComposite#getItemText(java.lang. Object)
			 */
			@Override
			public String getItemText(GUITestCase element) {
				return element.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DualDataSelectionListComposite#searchItems(java.lang. String)
			 */
			@Override
			public List<GUITestCase> searchItems(String filter) {
				return project.getAllGUITestCases().stream().filter(e -> e.getName().toLowerCase().contains(filter.toLowerCase()))
						.toList();
			}
		};

		listTestCases.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (editMode) {
			txtName.setText(testSuite.getName());
			txtComment.setText(testSuite.getComment());
			listTestCases.setSelectedItems(testSuite.getTestCases().stream().map(GUITestCase.class::cast).toList());
		}

		setTitle(title);
		setMessage("Enter test suite data");

		return panDialogArea;
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
				saveTestSuite();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
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

		// Check if another object with the same name already exists
		for (final JavaType type : testModule.getNamespace().getJavaTypes()) {
			if (editMode && type.equals(testSuite) && testSuite.getName().equals(txtName.getText()))
				continue;

			if (type.getName().equals(txtName.getText())) {
				setErrorMessage("A class with the same name already exists in this package!");
				txtName.setFocus();
				return false;
			}
		}

		return true;
	}

	/**
	 * Save the test suite
	 * @throws Exception if the save operation has failed
	 */
	private void saveTestSuite() throws Exception {
		if (editMode) {
			// Rename the existing test suite
			if (!testSuite.getName().equals(txtName.getText()))
				EclipseIDEService.renameCompUnit(testSuite.getSourceFile(), txtName.getText());
		}
		else
			testSuite.setNamespace(testModule.getNamespace());

		testSuite.setName(txtName.getText());
		testSuite.setComment(txtComment.getText());
		testSuite.getTestCases().clear();
		testSuite.getTestCases().addAll(listTestCases.getSelectedItems());

		final Resource eResource = testModule.getNamespace().eResource();

		if (!eResource.getContents().contains(testSuite))
			eResource.getContents().add(testSuite);

		EclipseIDEService.saveProjectMetaData(project);

		new TestSuiteService(project).rebuildTestSuiteSourceFile(testSuite);
	}

}
