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

import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.tdp.GUITestDataProposalService;
import net.codecadenza.eclipse.ui.dialog.testing.gui.util.GUITestActionUtil;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
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

/**
 * <p>
 * Dialog for maintaining test actions that are responsible for selecting a row
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RowSelectionDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_NEW = "Create new row selection action";
	private static final String DLG_TITLE_EDIT = "Edit row selection action";

	private String title = DLG_TITLE_NEW;
	private Button cmdDoubleClick;
	private Button cmdObjectId;
	private Button cmdCellValue;
	private Button cmdRowIndex;
	private AbstractProposalTextField<String> txtValue;
	private Button chkUsePaging;
	private FormAction formAction;
	private final Project project;
	private Form form;
	private GUITestAction rowSelectionAction;
	private boolean performDoubleClick;
	private Form selectedTargetForm;
	private boolean editMode;
	private final boolean enableDatabaseLookup;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param form
	 * @param formAction
	 * @param enableDatabaseLookup
	 */
	public RowSelectionDialog(Shell parentShell, Form form, FormAction formAction, boolean enableDatabaseLookup) {
		super(parentShell);

		this.formAction = formAction;
		this.form = form;
		this.project = form.getDomainObject().getNamespace().getProject();
		this.enableDatabaseLookup = enableDatabaseLookup;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param rowSelectionAction
	 * @param enableDatabaseLookup
	 */
	public RowSelectionDialog(Shell parentShell, Project project, GUITestAction rowSelectionAction, boolean enableDatabaseLookup) {
		super(parentShell);

		this.rowSelectionAction = rowSelectionAction;
		this.title = DLG_TITLE_EDIT;
		this.editMode = true;
		this.project = project;
		this.enableDatabaseLookup = enableDatabaseLookup;
	}

	/**
	 * @return the test action or null if no test action has been created
	 */
	public GUITestAction getRowSelectionAction() {
		return rowSelectionAction;
	}

	/**
	 * @return the selected target form
	 */
	public Form getTargetForm() {
		return selectedTargetForm;
	}

	/**
	 * @return true if a form action should be executed by performing a double-click
	 */
	public boolean performDoubleClick() {
		return performDoubleClick;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var groupRowFinder = new Group(panDialogArea, SWT.NONE);
		groupRowFinder.setLayout(new GridLayout(4, false));
		groupRowFinder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

		final var lblRowFinder = new Label(groupRowFinder, SWT.NONE);
		lblRowFinder.setText("Find row by using ");

		cmdCellValue = new Button(groupRowFinder, SWT.RADIO);
		cmdCellValue.setText("a cell value");

		cmdRowIndex = new Button(groupRowFinder, SWT.RADIO);
		cmdRowIndex.setText("a row index");

		cmdObjectId = new Button(groupRowFinder, SWT.RADIO);
		cmdObjectId.setText("the object ID");

		// Searching a row by using the object ID is currently not supported for Vaadin applications!
		cmdObjectId.setEnabled(!project.hasVaadinClient());

		final var lblName = new Label(groupRowFinder, SWT.NONE);
		lblName.setText("with value");

		txtValue = new AbstractProposalTextField<>(groupRowFinder, SWT.BORDER, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<String> getProposalData(String filter) {
				if (filter.isEmpty() || cmdRowIndex.getSelection())
					return Collections.emptyList();

				if (cmdCellValue.getSelection())
					return GUITestDataProposalService.searchProposals(project, filter);

				Form selectedForm = null;
				FormPanel selectedGridPanel = null;
				DTOBeanAttribute pkAttr = null;

				// When searching for a row by using the object ID the primary key DTO attribute of the respective form or grid panel must
				// be used to search for proposals!
				if (rowSelectionAction != null) {
					if (rowSelectionAction.getFormPanel() == null)
						selectedForm = rowSelectionAction.getForm();
					else
						selectedGridPanel = rowSelectionAction.getFormPanel();
				}

				if (formAction != null) {
					if (formAction.getPanel() == null)
						selectedForm = formAction.getForm();
					else
						selectedGridPanel = formAction.getPanel();
				}

				if (selectedForm != null)
					pkAttr = selectedForm.getDTO().getPKAttribute();

				if (selectedGridPanel != null)
					pkAttr = selectedGridPanel.getDTO().getPKAttribute();

				if (pkAttr != null)
					return GUITestDataProposalService.searchProposals(filter, pkAttr, enableDatabaseLookup);

				return Collections.emptyList();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang. Object)
			 */
			@Override
			public String getProposalLabel(String element) {
				return element;
			}
		};

		txtValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));

		chkUsePaging = new Button(groupRowFinder, SWT.CHECK);
		chkUsePaging.setText("enable searching row in all available pages");
		chkUsePaging.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 4, 1));

		if (!editMode) {
			cmdCellValue.setSelection(true);

			// Data that doesn't belong to the row selection cannot be maintained in edit mode!
			final var groupActionExec = new Group(panDialogArea, SWT.NONE);
			groupActionExec.setLayout(new GridLayout(3, false));
			groupActionExec.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

			final var lblActionExec = new Label(groupActionExec, SWT.NONE);
			lblActionExec.setText("Execute action by");

			final var cmdContextMenuItem = new Button(groupActionExec, SWT.RADIO);
			cmdContextMenuItem.setText("using context-menu");
			cmdContextMenuItem.setSelection(true);

			cmdDoubleClick = new Button(groupActionExec, SWT.RADIO);
			cmdDoubleClick.setText("performing a double click");
			cmdDoubleClick.setEnabled(false);

			cmdDoubleClick.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					performDoubleClick = cmdDoubleClick.getSelection();
				}
			});

			final var lblTargetForm = new Label(panDialogArea, SWT.NONE);
			lblTargetForm.setText("Target form:");

			final var cboTargetForm = new DataComboViewer<Form>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
				 */
				@Override
				public String getItemText(Form element) {
					return element.getName();
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
				 */
				@Override
				public void onSelectionChanged(Form element) {
					if (element.getName().isEmpty())
						selectedTargetForm = null;
					else
						selectedTargetForm = element;
				}
			};

			cboTargetForm.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (formAction.getType() == ActionType.COPY) {
				// Determine the available target forms after performing a copy action
				final DomainObject domainObject = formAction.getForm().getDomainObject();

				final List<Form> formList = project.getAllFormsOfProject().stream().filter(e -> e.getDomainObject().equals(domainObject))
						.filter(e -> e.getFormType() == FormTypeEnumeration.UPDATE && !project.hasAngularClient())
						.collect(Collectors.toList());

				// Add a temporary form with an empty name in order to be able to cover situations where a copy operation won't navigate
				// to a corresponding 'UPDATE' form!
				final Form emptyForm = ClientFactory.eINSTANCE.createForm();
				emptyForm.setName("");

				formList.add(emptyForm);

				cboTargetForm.setData(formList);
				cboTargetForm.setSelectedItem(formList.get(0));
			}
			else if (formAction.getTargetForm() != null) {
				final Form targetForm = formAction.getTargetForm();
				final FormTypeEnumeration formType = targetForm.getFormType();

				cboTargetForm.setData(Arrays.asList(targetForm));
				cboTargetForm.setSelectedItem(targetForm);

				if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
					cmdDoubleClick.setEnabled(true);
			}
		}
		else {
			final GUITestData testDataObjectId = rowSelectionAction.getTestDataByType(GUITestDataType.OBJECT_ID);
			final GUITestData testDataCellValue = rowSelectionAction.getTestDataByType(GUITestDataType.CELL_VALUE);
			final GUITestData testDataRowIndex = rowSelectionAction.getTestDataByType(GUITestDataType.ROW_INDEX);

			if (testDataObjectId != null) {
				cmdObjectId.setSelection(true);
				txtValue.getControl().setText(testDataObjectId.getFilterValue() != null ? testDataObjectId.getFilterValue() : "");
			}
			else if (testDataCellValue != null) {
				cmdCellValue.setSelection(true);
				txtValue.getControl().setText(testDataCellValue.getFilterValue() != null ? testDataCellValue.getFilterValue() : "");
			}
			else if (testDataRowIndex != null) {
				cmdRowIndex.setSelection(true);
				txtValue.getControl().setText(testDataRowIndex.getFilterValue() != null ? testDataRowIndex.getFilterValue() : "");
			}

			chkUsePaging.setSelection(rowSelectionAction.getType() == GUITestActionType.SEARCH_ROW_ALL_PAGES);
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// A filter value must be entered in any case!
			if (txtValue.getControl().getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The row filter value must not be empty!");
				txtValue.getControl().setFocus();
				return;
			}

			if (cmdRowIndex.getSelection()) {
				// The row index must be an integer!
				try {
					Integer.parseInt(txtValue.getControl().getText());
				}
				catch (final NumberFormatException _) {
					MessageDialog.openInformation(getShell(), title, "The row index requires an integer value!");
					txtValue.getControl().setFocus();
					return;
				}
			}

			if (!editMode) {
				rowSelectionAction = TestingFactory.eINSTANCE.createGUITestAction();
				rowSelectionAction.setForm(form);
				rowSelectionAction.setFormPanel(formAction.getPanel());
				rowSelectionAction.setFormAction(formAction);
			}

			if (chkUsePaging.getSelection())
				rowSelectionAction.setType(GUITestActionType.SEARCH_ROW_ALL_PAGES);
			else
				rowSelectionAction.setType(GUITestActionType.SEARCH_ROW_CURRENT_PAGE);

			// Create a comment
			GUITestActionUtil.initTestActionComment(rowSelectionAction);

			GUITestData selectionTestData = null;

			if (!rowSelectionAction.getTestData().isEmpty())
				selectionTestData = rowSelectionAction.getTestData().get(0);
			else
				selectionTestData = TestingFactory.eINSTANCE.createGUITestData();

			selectionTestData.setTestAction(rowSelectionAction);

			if (cmdObjectId.getSelection())
				selectionTestData.setType(GUITestDataType.OBJECT_ID);
			else if (cmdCellValue.getSelection())
				selectionTestData.setType(GUITestDataType.CELL_VALUE);
			else
				selectionTestData.setType(GUITestDataType.ROW_INDEX);

			selectionTestData.setFilterValue(txtValue.getControl().getText());
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

		newShell.setText(title);
	}

}
