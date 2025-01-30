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
package net.codecadenza.eclipse.service.testing.gui;

import static net.codecadenza.eclipse.shared.Constants.PACK_PAGE_OBJECT;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.generator.testing.gui.GUITestGenerator;
import net.codecadenza.eclipse.generator.testing.gui.GUITestGeneratorFactory;
import net.codecadenza.eclipse.generator.testing.gui.imp.SeleniumFormPageObjectGenerator;
import net.codecadenza.eclipse.generator.testing.gui.imp.SeleniumGridPanelPageObjectGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaTestFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.service.testing.TestSuiteService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service for GUI test cases
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestCaseService {
	private final Project project;
	private SeleniumTestModule seleniumTestModule;

	/**
	 * Constructor
	 * @param project
	 */
	public GUITestCaseService(Project project) {
		this.project = project;

		final AbstractTestModule testModule = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);

		if (testModule != null)
			this.seleniumTestModule = (SeleniumTestModule) testModule;
	}

	/**
	 * Rebuild the test case source file and the corresponding test data file
	 * @param testCase
	 * @throws Exception if the rebuild operation has failed due to an internal error
	 */
	public void rebuildTestCaseSourceFiles(GUITestCase testCase) throws Exception {
		final GUITestGenerator generator = GUITestGeneratorFactory.getGenerator(testCase.getTestModule());

		// Create or rebuild the test case
		generator.createTestCase(testCase);

		// Create or rebuild the file that contains test case data
		generator.createTestData(testCase);
	}

	/**
	 * Rebuild all page objects
	 * @throws Exception if the rebuild operation has failed due to an internal error
	 */
	public void rebuildAllPageObjectSourceFiles() throws Exception {
		// Search for all forms and rebuild respective page objects
		for (final Form form : project.getAllFormsOfProject())
			rebuildPageObject(form);

		// Rebuild page objects that represent grid panels
		for (final FormPanel gridPanel : project.getAllGridPanelsOfProject())
			rebuildPageObject(gridPanel);
	}

	/**
	 * Rename the given test case
	 * @param testCase
	 * @param newName
	 * @throws Exception if the rename operation has failed
	 */
	public void renameTestCase(GUITestCase testCase, String newName) throws Exception {
		final BuildArtifactType artifactType = testCase.getTestModule().getArtifactType();

		// Rename the test case source file
		EclipseIDEService.renameCompUnit(testCase.getSourceFile(), newName);

		// Delete the test data file
		final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";
		final String projectName = project.getTargetProjectName(artifactType);

		EclipseIDEService.deleteWorkspaceFile(projectName, path);

		testCase.setName(newName);

		// Create a new test data file
		final GUITestGenerator generator = GUITestGeneratorFactory.getGenerator(testCase.getTestModule());
		generator.createTestData(testCase);
	}

	/**
	 * Delete the given test case and remove respective files
	 * @param testCase
	 * @throws Exception if the delete operation has failed due to an internal error
	 */
	public void deleteTestCase(GUITestCase testCase) throws Exception {
		final BuildArtifactType artifactType = testCase.getTestModule().getArtifactType();
		final List<TestSuite> testSuites = testCase.getTestSuites();
		final var testSuiteService = new TestSuiteService(project);

		// Remove the test case from the meta-model
		project.eResource().getContents().remove(testCase);

		EclipseIDEService.saveProjectMetaData(project);

		// Delete the test case source file
		EclipseIDEService.deleteSource(testCase.getSourceFile());

		// Delete the test data file
		final String projectName = project.getTargetProjectName(artifactType);
		final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";

		EclipseIDEService.deleteWorkspaceFile(projectName, path);

		// Remove the test case from respective test suites
		testSuites.forEach(e -> e.getTestCases().remove(testCase));

		// Rebuild all test suites that previously contained this test case
		for (final TestSuite testSuite : testSuites)
			testSuiteService.rebuildTestSuiteSourceFile(testSuite);
	}

	/**
	 * Synchronize dependent test cases when deleting a form
	 * @param form
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnDeleteForm(Form form) throws Exception {
		final var testCases = new ArrayList<GUITestCase>();

		if (seleniumTestModule == null)
			return;

		// Search for all test actions that reference the given form
		seleniumTestModule.getGUITestCases().forEach(testCase -> {
			final var testActions = new ArrayList<GUITestAction>();

			for (final GUITestAction testAction : testCase.getTestActions())
				if (form.equals(testAction.getForm()) || form.equals(testAction.getTargetForm()))
					testActions.add(testAction);

			testCase.getTestActions().removeAll(testActions);
			testCases.add(testCase);
		});

		// Delete the page object that represents this form
		final JavaFile pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			EclipseIDEService.deleteSource(pageObjectFile);

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test cases when removing a grid panel from a form
	 * @param formPanel
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnRemoveGridPanel(FormPanel formPanel) throws Exception {
		final FormPanel gridPanel = formPanel.getBasePanel();
		final var testCases = new ArrayList<GUITestCase>();

		if (seleniumTestModule == null)
			return;

		// Search for all test actions that reference the given grid panel
		seleniumTestModule.getGUITestCases().forEach(testCase -> {
			final var testActions = new ArrayList<GUITestAction>();

			for (final GUITestAction testAction : testCase.getTestActions())
				if (gridPanel.equals(testAction.getFormPanel()))
					testActions.add(testAction);

			testCase.getTestActions().removeAll(testActions);
			testCases.add(testCase);
		});

		// Rebuild the page object that represents the respective form
		final JavaFile pageObjectFile = initPageObject(formPanel.getForm());

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(formPanel.getForm()).createSourceFile();

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test cases when deleting a grid panel
	 * @param gridPanel
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnDeleteGridPanel(FormPanel gridPanel) throws Exception {
		if (seleniumTestModule == null)
			return;

		// Delete the page object that represents the grid panel
		final JavaFile pageObjectFile = initPageObject(gridPanel);

		if (pageObjectFile != null)
			EclipseIDEService.deleteSource(pageObjectFile);
	}

	/**
	 * Synchronize dependent test cases when editing a form
	 * @param form
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnEditForm(Form form) throws Exception {
		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test cases that reference the given form
		for (final GUITestCase testCase : seleniumTestModule.getGUITestCases())
			for (final GUITestAction testAction : testCase.getTestActions())
				if (form.equals(testAction.getForm())) {
					testCases.add(testCase);
					break;
				}

		// Rebuild the page object that represents this form
		final JavaFile pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(form).createSourceFile();

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test source files when creating a new form
	 * @param form
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnCreateForm(Form form) throws Exception {
		if (seleniumTestModule == null)
			return;

		// When creating a new form the respective page object must be created also
		rebuildPageObject(form);
	}

	/**
	 * Synchronize dependent test source files when creating a new grid panel
	 * @param gridPanel
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnCreateGridPanel(FormPanel gridPanel) throws Exception {
		if (seleniumTestModule == null)
			return;

		// When creating a new grid panel the respective page object must be created also
		rebuildPageObject(gridPanel);
	}

	/**
	 * Synchronize dependent test source files when renaming an existing form
	 * @param form
	 * @param oldName
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnRenameForm(Form form, String oldName) throws Exception {
		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test cases that reference the given form
		for (final GUITestCase testCase : seleniumTestModule.getGUITestCases())
			for (final GUITestAction testAction : testCase.getTestActions())
				if (form.equals(testAction.getForm()) || form.equals(testAction.getTargetForm())) {
					testCases.add(testCase);
					break;
				}

		// Delete the page object
		JavaFile pageObjectFile = initPageObject(form, oldName);

		if (pageObjectFile != null)
			EclipseIDEService.deleteSource(pageObjectFile);

		// Create new page object source file
		pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(form).createSourceFile();

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test source files when renaming a grid panel
	 * @param gridPanel
	 * @param oldName
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnRenameGridPanel(FormPanel gridPanel, String oldName) throws Exception {
		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test cases that reference the given grid panel
		for (final GUITestCase testCase : seleniumTestModule.getGUITestCases())
			for (final GUITestAction testAction : testCase.getTestActions())
				if (gridPanel.equals(testAction.getFormPanel())) {
					testCases.add(testCase);
					break;
				}

		// Delete the page object
		JavaFile pageObjectFile = initPageObject(gridPanel, oldName);

		if (pageObjectFile != null)
			EclipseIDEService.deleteSource(pageObjectFile);

		// Create a new page object source file
		pageObjectFile = initPageObject(gridPanel);

		if (pageObjectFile != null)
			new SeleniumGridPanelPageObjectGenerator(gridPanel).createSourceFile();

		// Search for all forms that reference this grid panel and rebuild respective page objects
		for (final Form form : seleniumTestModule.getProject().getAllFormsOfProject())
			for (final FormPanel formPanel : form.getFormPanels())
				if (formPanel.getBasePanel() != null && formPanel.getBasePanel().equals(gridPanel)) {
					rebuildPageObject(form);
					break;
				}

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test source files when deleting a form field
	 * @param formField
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnDeleteField(FormField formField) throws Exception {
		final Form form = formField.getPanel().getForm();

		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test data objects that reference the given form field
		seleniumTestModule.getGUITestCases().forEach(testCase -> testCase.getTestActions().forEach(testAction -> {
			final GUITestData testDataToDelete = testAction.getTestData().stream()
					.filter(testData -> formField.equals(testData.getFormField())).findFirst().orElse(null);

			// A test data object that references the given form field must be deleted!
			if (testDataToDelete != null) {
				testAction.getTestData().remove(testDataToDelete);
				testCases.add(testCase);
			}
		}));

		// Rebuild the page object that represents this form
		final JavaFile pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(form).createSourceFile();

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test source files when deleting a column of a view form
	 * @param column
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnDeleteColumn(TableColumnField column) throws Exception {
		final Form form = column.getFormTable().getFormPanel().getForm();

		// Omit further operations if the column belongs to a grid panel!
		if (form == null)
			return;

		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test data objects that reference the given table column
		seleniumTestModule.getGUITestCases().forEach(testCase -> testCase.getTestActions().forEach(testAction -> {
			final var testDataToDelete = new ArrayList<GUITestData>();

			for (final GUITestData testData : testAction.getTestData())
				if (column.equals(testData.getTableColumnField()))
					testDataToDelete.add(testData);

			// Remove all test data objects that reference the given table column!
			if (!testDataToDelete.isEmpty()) {
				testAction.getTestData().removeAll(testDataToDelete);
				testCases.add(testCase);
			}
		}));

		// Rebuild the page object that represents this form
		final JavaFile pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(form).createSourceFile();

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test source files when deleting a form action
	 * @param form
	 * @param formAction
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnDeleteFormAction(Form form, FormAction formAction) throws Exception {
		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test actions that reference the given form action
		seleniumTestModule.getGUITestCases().forEach(testCase -> {
			final var testActions = new ArrayList<GUITestAction>();

			for (final GUITestAction testAction : testCase.getTestActions())
				if (formAction.equals(testAction.getFormAction()))
					testActions.add(testAction);

			// Remove all test actions that reference the given form action!
			if (!testActions.isEmpty()) {
				testCase.getTestActions().removeAll(testActions);
				testCases.add(testCase);
			}
		});

		// Rebuild the page object that represents this form
		final JavaFile pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(form).createSourceFile();

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Synchronize dependent test source files when deleting a grid panel action
	 * @param formAction
	 * @throws Exception if the synchronization has failed due to an internal error
	 */
	public void syncOnDeleteFormAction(FormAction formAction) throws Exception {
		if (seleniumTestModule == null)
			return;

		final var testCases = new HashSet<GUITestCase>();

		// Search for all test actions that reference the given form action
		seleniumTestModule.getGUITestCases().forEach(testCase -> {
			final var testActions = new ArrayList<GUITestAction>();

			for (final GUITestAction testAction : testCase.getTestActions())
				if (formAction.equals(testAction.getFormAction()))
					testActions.add(testAction);

			// Remove all test actions that reference the given form action!
			if (!testActions.isEmpty()) {
				testCase.getTestActions().removeAll(testActions);
				testCases.add(testCase);
			}
		});

		// Rebuild all affected test cases
		for (final GUITestCase testCase : testCases)
			rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Rebuild the page object that represents the given form
	 * @param form
	 * @throws Exception if the rebuild operation has failed due to an internal error
	 */
	private void rebuildPageObject(Form form) throws Exception {
		if (seleniumTestModule == null)
			return;

		final JavaFile pageObjectFile = initPageObject(form);

		if (pageObjectFile != null)
			new SeleniumFormPageObjectGenerator(form).createSourceFile();
	}

	/**
	 * Rebuild the page object that represents the given grid panel
	 * @param gridPanel
	 * @throws Exception if the rebuild operation has failed due to an internal error
	 */
	private void rebuildPageObject(FormPanel gridPanel) throws Exception {
		if (seleniumTestModule == null)
			return;

		final JavaFile pageObjectFile = initPageObject(gridPanel);

		if (pageObjectFile != null)
			new SeleniumGridPanelPageObjectGenerator(gridPanel).createSourceFile();
	}

	/**
	 * Create an internal representation of a page object that represents the given form
	 * @param form
	 * @return the internal representation of a page object or null if a page object for the given form is not necessary
	 */
	private JavaFile initPageObject(Form form) {
		return initPageObject(form, null);
	}

	/**
	 * Create an internal representation of a page object that represents the given form
	 * @param form
	 * @param name
	 * @return the internal representation of a page object or null if a page object for the given form is not necessary
	 */
	private JavaFile initPageObject(Form form, String name) {
		if (seleniumTestModule == null)
			return null;

		if (form.getFormType() == FormTypeEnumeration.LOV || form.getFormType() == FormTypeEnumeration.TREE_VIEW)
			return null;

		// The page object should not be created as long as the corresponding form has not been added to the meta-model!
		if (!seleniumTestModule.getProject().eResource().getContents().contains(form))
			return null;

		final String pageObjectName = name == null ? form.getName() : name;
		final String pageObjectPackage = seleniumTestModule.getNamespace().toString() + PACK_PAGE_OBJECT;

		return new JavaTestFile(project, seleniumTestModule.getArtifactType(), pageObjectName, pageObjectPackage);
	}

	/**
	 * Create an internal representation of a page object that represents the given grid panel
	 * @param gridPanel
	 * @return the internal representation of a page object or null if a page object for the given grid panel is not necessary
	 */
	private JavaFile initPageObject(FormPanel gridPanel) {
		return initPageObject(gridPanel, null);
	}

	/**
	 * Create an internal representation of a page object that represents the given grid panel
	 * @param gridPanel
	 * @param name
	 * @return the internal representation of a page object or null if a page object for the given grid panel is not necessary
	 */
	private JavaFile initPageObject(FormPanel gridPanel, String name) {
		if (seleniumTestModule == null)
			return null;

		// The page object should not be created as long as the corresponding grid panel has not been added to the meta-model!
		if (!seleniumTestModule.getProject().eResource().getContents().contains(gridPanel))
			return null;

		final String pageObjectName = name == null ? gridPanel.getName() : name;
		final String pageObjectPackage = seleniumTestModule.getNamespace().toString() + PACK_PAGE_OBJECT;

		return new JavaTestFile(project, seleniumTestModule.getArtifactType(), pageObjectName, pageObjectPackage);
	}

}
