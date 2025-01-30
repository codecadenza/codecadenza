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
package net.codecadenza.eclipse.ui.dialog.testing.gui.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.GUITestFormEditorDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.RowSelectionDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Utility class for GUI test actions
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestActionUtil {
	/**
	 * Prevent instantiation
	 */
	private GUITestActionUtil() {

	}

	/**
	 * Initialize additional test actions that are necessary for single-record forms
	 * @param form
	 * @param formAction
	 * @param enableDatabaseLookup
	 * @return a list containing all GUI test actions
	 */
	public static List<GUITestAction> initGUITestActionsOfForm(Form form, FormAction formAction, boolean enableDatabaseLookup) {
		final Project project = form.getDomainObject().getNamespace().getProject();
		final var actions = new ArrayList<GUITestAction>();
		final Shell shell = Display.getCurrent().getActiveShell();

		final GUITestAction pageDataAction = TestingFactory.eINSTANCE.createGUITestAction();
		pageDataAction.setForm(form);
		pageDataAction.setFormAction(formAction);

		if (form.getFormType() == FormTypeEnumeration.READONLY)
			pageDataAction.setType(GUITestActionType.VALIDATE_FORM_DATA);
		else
			pageDataAction.setType(GUITestActionType.ENTER_FORM_DATA);

		// Create a comment
		initTestActionComment(pageDataAction);

		final var dlg = new GUITestFormEditorDialog(shell, pageDataAction, form, true, enableDatabaseLookup);
		final int returnCode = dlg.open();

		actions.addAll(dlg.getPreviewBuilder().getTestActions());
		actions.add(pageDataAction);

		// Create a new test action regarding the selected button that has been clicked in the editor
		final GUITestAction buttonClickAction = TestingFactory.eINSTANCE.createGUITestAction();
		buttonClickAction.setForm(form);
		buttonClickAction.setFormAction(formAction);

		if (returnCode != Window.OK || form.getFormType() == FormTypeEnumeration.READONLY)
			buttonClickAction.setType(GUITestActionType.PRESS_CANCEL_BUTTON);
		else
			buttonClickAction.setType(GUITestActionType.PRESS_OK_BUTTON);

		// Create a comment
		initTestActionComment(buttonClickAction);

		actions.add(buttonClickAction);

		// Test if a form of type 'UPDATE' is opened automatically after pressing the 'OK' button
		if (form.isOpenEditAfterCreate()
				&& (form.getFormType() == FormTypeEnumeration.ADD || form.getFormType() == FormTypeEnumeration.CREATE)) {
			final Optional<Form> updateForm = project.getAllFormsOfProject().stream()
					.filter(f -> f.getFormType() == FormTypeEnumeration.UPDATE && form.getDomainObject().equals(f.getDomainObject()))
					.findFirst();

			if (updateForm.isPresent()) {
				buttonClickAction.setTargetForm(updateForm.get());

				// Initialize the test actions for this form
				actions.addAll(initGUITestActionsOfForm(updateForm.get(), formAction, enableDatabaseLookup));
			}
		}

		return actions;
	}

	/**
	 * Initialize test actions based on a form action
	 * @param form
	 * @param formAction
	 * @param enableDatabaseLookup
	 * @return a list containing all GUI test actions
	 */
	public static List<GUITestAction> initGUITestActionsOfFormAction(Form form, FormAction formAction,
			boolean enableDatabaseLookup) {
		final var testActions = new ArrayList<GUITestAction>();
		final Shell shell = Display.getCurrent().getActiveShell();

		final GUITestAction toolBarAction = TestingFactory.eINSTANCE.createGUITestAction();
		toolBarAction.setForm(form);
		toolBarAction.setFormAction(formAction);
		toolBarAction.setFormPanel(formAction.getPanel());
		toolBarAction.setTargetForm(formAction.getTargetForm());
		toolBarAction.setType(GUITestActionType.EXECUTE_FORM_ACTION);

		if (formAction.getType() == ActionType.CREATE) {
			testActions.add(toolBarAction);
			testActions.addAll(initGUITestActionsOfForm(formAction.getTargetForm(), formAction, enableDatabaseLookup));
		}
		else if (formAction.getType() == ActionType.DIRECT_UPLOAD || formAction.getType() == ActionType.INDIRECT_UPLOAD) {
			final var dlg = new FileDialog(shell, SWT.OPEN | SWT.SINGLE);
			dlg.setText("Select a file for upload");

			final String pathToFile = dlg.open();

			if (pathToFile == null)
				return testActions;

			toolBarAction.setType(GUITestActionType.UPLOAD_FILE);

			final GUITestData testData = TestingFactory.eINSTANCE.createGUITestData();
			testData.setTestAction(toolBarAction);
			testData.setType(GUITestDataType.FORM_FIELD);
			testData.setNewValue(pathToFile);
			testData.setTestAction(toolBarAction);

			testActions.add(toolBarAction);
		}
		else if (formAction.getType() == ActionType.UPLOAD_IMPORT) {
			final var exchangeMethod = (DataExchangeMethod) formAction.getBoundaryMethod().getServiceMethod();
			String pathToFile = null;

			// A dialog won't be necessary if an import operation just has to be started!
			if (!exchangeMethod.getMethodParameters().isEmpty()) {
				final var dlg = new FileDialog(shell, SWT.OPEN | SWT.SINGLE);
				dlg.setText("Select an import file");

				pathToFile = dlg.open();

				if (pathToFile == null)
					return testActions;
			}

			if (pathToFile != null) {
				final GUITestData testData = TestingFactory.eINSTANCE.createGUITestData();
				testData.setTestAction(toolBarAction);
				testData.setType(GUITestDataType.FORM_FIELD);
				testData.setNewValue(pathToFile);
				testData.setTestAction(toolBarAction);
			}

			testActions.add(toolBarAction);
		}
		else if (formAction.getType() == ActionType.DOWNLOAD_EXPORT) {
			final var exchangeMethod = (DataExchangeMethod) formAction.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.getSingleObjectFilterParam() != null) {
				final var dlg = new RowSelectionDialog(shell, form, formAction, enableDatabaseLookup);
				final int returnCode = dlg.open();

				if (returnCode != Window.OK)
					return testActions;

				testActions.add(dlg.getRowSelectionAction());
			}

			testActions.add(toolBarAction);
		}
		else if (formAction.getType() == ActionType.DOWNLOAD) {
			if (formAction.getPanel() != null || form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW
					|| form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW) {
				final var dlg = new RowSelectionDialog(shell, form, formAction, enableDatabaseLookup);
				final int returnCode = dlg.open();

				if (returnCode != Window.OK)
					return testActions;

				testActions.add(dlg.getRowSelectionAction());
			}
			else
				toolBarAction.setType(GUITestActionType.PRESS_DOWNLOAD_BUTTON);

			testActions.add(toolBarAction);
		}
		else if (formAction.getType() == ActionType.DELETE) {
			final var dlg = new RowSelectionDialog(shell, form, formAction, enableDatabaseLookup);
			final int returnCode = dlg.open();

			if (returnCode != Window.OK)
				return testActions;

			testActions.add(dlg.getRowSelectionAction());
			testActions.add(toolBarAction);
		}
		else if (formAction.getType() == ActionType.COPY) {
			final var dlg = new RowSelectionDialog(shell, form, formAction, enableDatabaseLookup);
			final int returnCode = dlg.open();

			if (returnCode != Window.OK)
				return testActions;

			testActions.add(dlg.getRowSelectionAction());
			testActions.add(toolBarAction);

			if (dlg.getTargetForm() != null) {
				toolBarAction.setTargetForm(dlg.getTargetForm());

				testActions.addAll(initGUITestActionsOfForm(dlg.getTargetForm(), formAction, enableDatabaseLookup));
			}
			else
				toolBarAction.setTargetForm(null);
		}
		else if (formAction.getTargetForm() != null) {
			final var dlg = new RowSelectionDialog(shell, form, formAction, enableDatabaseLookup);
			final int returnCode = dlg.open();

			if (returnCode != Window.OK)
				return testActions;

			toolBarAction.setTargetForm(dlg.getTargetForm());

			if (dlg.performDoubleClick())
				toolBarAction.setType(GUITestActionType.DOUBLE_CLICK_ROW);

			testActions.add(dlg.getRowSelectionAction());
			testActions.add(toolBarAction);
			testActions.addAll(initGUITestActionsOfForm(formAction.getTargetForm(), formAction, enableDatabaseLookup));
		}

		// Create a comment
		initTestActionComment(toolBarAction);

		return testActions;
	}

	/**
	 * Initialize and set the comment for a given GUI test action
	 * @param testAction
	 */
	public static void initTestActionComment(GUITestAction testAction) {
		final FormAction formAction = testAction.getFormAction();
		final GUITestActionType type = testAction.getType();
		final Form form = testAction.getForm();
		final var formName = form != null ? form.getName() : "";
		var targetFormName = "";
		var comment = "";

		if (formAction != null && formAction.getTargetForm() != null)
			targetFormName = formAction.getTargetForm().getName();

		if (formAction != null && type == GUITestActionType.EXECUTE_FORM_ACTION) {
			if (formAction.getType() == ActionType.UPLOAD_IMPORT)
				comment = "Perform data import by using context-menu of" + generateActionSourceComment(testAction);
			else if (formAction.getType() == ActionType.DOWNLOAD_EXPORT)
				comment = "Perform data export by using context-menu of" + generateActionSourceComment(testAction);
			else if (formAction.getType() == ActionType.DOWNLOAD)
				comment = "Download file by using context-menu of" + generateActionSourceComment(testAction);
			else if (formAction.getType() == ActionType.DELETE)
				comment = "Delete selected row by using context-menu of" + generateActionSourceComment(testAction);
			else if (formAction.getType() == ActionType.COPY)
				comment = "Copy selected object by using context-menu of" + generateActionSourceComment(testAction);
			else if (formAction.getTargetForm() != null) {
				final Project project = formAction.getTargetForm().getDTO().getNamespace().getProject();
				final FormTypeEnumeration targetFormType = formAction.getTargetForm().getFormType();
				comment = "Open page '" + targetFormName + "' by ";

				if (targetFormType == FormTypeEnumeration.CREATE) {
					if (project.hasVaadinClient())
						comment += "pressing button 'Create new'";
					else if (project.hasAngularClient())
						comment += "pressing button 'New'";
					else
						comment += "clicking on menu bar item 'Create new'";
				}
				else if (targetFormType == FormTypeEnumeration.ADD) {
					if (project.hasVaadinClient())
						comment += "pressing button 'Create new'";
					else if (project.hasAngularClient())
						comment += "pressing button 'New'";
					else
						comment += "clicking on menu bar item 'Add'";
				}
				else
					comment += "clicking on the respective context-menu item";
			}
		}
		else if (type == GUITestActionType.EXECUTE_REFRESH_ACTION) {
			if (testAction.getFormPanel() == null)
				comment = "Refresh page '" + formName + "'";
			else
				comment = "Refresh grid panel '" + testAction.getFormPanel().getName() + "'";
		}
		else if (type == GUITestActionType.DOUBLE_CLICK_ROW)
			comment = "Open page '" + targetFormName + "' by performing a double-click";
		else if (type == GUITestActionType.UPLOAD_FILE)
			comment = "Press file upload button of" + generateActionSourceComment(testAction);
		else if (type == GUITestActionType.PRESS_DOWNLOAD_BUTTON)
			comment = "Press file download button of" + generateActionSourceComment(testAction);
		else if (type == GUITestActionType.OPEN_LOGIN_PAGE)
			comment = "Open login page and enter credentials";
		else if (type == GUITestActionType.PERFORM_LOGOUT)
			comment = "Perform logout by pressing respective button";
		else if (type == GUITestActionType.OPEN_PAGE_DIRECT)
			comment = "Open page '" + formName + "' directly";
		else if (type == GUITestActionType.OPEN_PAGE_BY_NAVIGATOR)
			comment = "Open page '" + formName + "' by using main navigator";
		else if (type == GUITestActionType.ENTER_SEARCH_DATA)
			comment = "Open search dialog of page '" + formName + "'";
		else if (type == GUITestActionType.COUNT_RECORDS)
			comment = "Perform count operation for page '" + formName + "'";
		else if (type == GUITestActionType.RESET_SEARCH_DATA)
			comment = "Reset search input dialog of page '" + formName + "'";
		else if (type == GUITestActionType.VALIDATE_ROW_COUNT_EQUAL || type == GUITestActionType.VALIDATE_ROW_COUNT_GREATER
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_SMALLER)
			comment = "Validate the number of displayed rows of" + generateActionSourceComment(testAction);
		else if (type == GUITestActionType.SEARCH_ROW_ALL_PAGES)
			comment = "Search for row in" + generateActionSourceComment(testAction) + " (with pagination)";
		else if (type == GUITestActionType.SEARCH_ROW_CURRENT_PAGE)
			comment = "Search for row in" + generateActionSourceComment(testAction);
		else if (type == GUITestActionType.VALIDATE_FORM_DATA)
			comment = "Validate data of page '" + formName + "'";
		else if (type == GUITestActionType.ENTER_FORM_DATA)
			comment = "Enter data into page '" + formName + "'";
		else if (type == GUITestActionType.PRESS_CANCEL_BUTTON)
			comment = "Press button to close page '" + formName + "'";
		else if (type == GUITestActionType.PRESS_OK_BUTTON)
			comment = "Press 'OK' button of page '" + formName + "'";

		testAction.setComment(comment);
	}

	/**
	 * Create a comment fragment that describes if a test action belongs to either a form or a grid panel
	 * @param testAction
	 * @return the generated string
	 */
	private static final String generateActionSourceComment(GUITestAction testAction) {
		final FormPanel formPanel = testAction.getFormPanel();
		final Form form = testAction.getForm();

		if (formPanel != null)
			return " grid panel '" + formPanel.getName() + "'";

		if (form != null)
			return " page '" + form.getName() + "'";

		return "";
	}

}
