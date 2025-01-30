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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.tdp.GUITestDataProposalService;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.FontDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for entering search data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchInputDialog extends CodeCadenzaDialog {
	private static final String SORT_ORDER_ASC = "ASC";
	private static final String SORT_ORDER_DESC = "DESC";
	private static final String OPERATOR_BETWEEN = "between";
	private static final String OPERATOR_IN = "in";
	private static final String OPERATOR_NOT_IN = "not in";
	private static final String OPERATOR_IS_NULL = "is null";
	private static final String OPERATOR_IS_NOT_NULL = "is not null";
	private static final String OPERATOR_LIKE = "like";
	private static final String OPERATOR_NOT_LIKE = "not like";
	private static final String OPERATOR_EQUAL = "equal";
	private static final String OPERATOR_GREATER = "greater";
	private static final String OPERATOR_SMALLER = "smaller";
	private static final String OPERATOR_GREATER_OR_EQUAL = "greater or equal";
	private static final String OPERATOR_SMALLER_OR_EQUAL = "smaller or equal";

	private final GUITestAction testAction;
	private final List<TableColumnField> fields;
	private final boolean enableAllActionTypes;
	private final boolean enableDatabaseLookup;
	private final Project project;
	private Font fontBold;
	private GUITestActionType selectedActionType;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testAction
	 * @param enableAllActionTypes
	 * @param enableDatabaseLookup
	 */
	public SearchInputDialog(Shell parentShell, GUITestAction testAction, boolean enableAllActionTypes,
			boolean enableDatabaseLookup) {
		super(parentShell);

		this.testAction = testAction;
		this.project = testAction.getForm().getDomainObject().getNamespace().getProject();
		this.enableAllActionTypes = enableAllActionTypes;
		this.enableDatabaseLookup = enableDatabaseLookup;

		if (this.project.hasVaadinClient()) {
			// Non-searchable fields are not supported for Vaadin applications. Thus, they must be added to the list!
			this.fields = testAction.getForm().getFormPanels().get(0).getFormTable().getFields().stream()
					.filter(TableColumnField::isVisible).toList();
		}
		else
			this.fields = testAction.getForm().getFormPanels().get(0).getFormTable().getFields().stream()
					.filter(e -> e.isSearchable() && e.isVisible()).toList();
	}

	/**
	 * @return the selected GUI test action type
	 */
	public GUITestActionType getSelectedActionType() {
		return selectedActionType;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var scrolledComposite = new ScrolledComposite(panDialogArea, SWT.BORDER | SWT.V_SCROLL);
		scrolledComposite.setExpandHorizontal(true);
		scrolledComposite.setAlwaysShowScrollBars(true);
		scrolledComposite.setLayout(new FillLayout());
		scrolledComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		scrolledComposite.setMinWidth(800);
		scrolledComposite.setMinHeight(600);

		final var glFilter = new GridLayout(4, false);
		glFilter.horizontalSpacing = 2;
		glFilter.verticalSpacing = 2;

		// Composite for filter criteria
		final var panFilter = new Composite(scrolledComposite, SWT.NONE);
		panFilter.setLayout(glFilter);

		final FontDescriptor boldDescriptor = FontDescriptor.createFrom(panDialogArea.getFont()).setStyle(SWT.BOLD);

		fontBold = boldDescriptor.createFont(panDialogArea.getDisplay());

		final var lblFieldTitle = new Label(panFilter, SWT.NONE);
		lblFieldTitle.setText("Field title");
		lblFieldTitle.setFont(fontBold);

		final var lblFieldOperator = new Label(panFilter, SWT.NONE);
		lblFieldOperator.setText("Operator");
		lblFieldOperator.setFont(fontBold);

		final var lblFieldSortOrder = new Label(panFilter, SWT.NONE);
		lblFieldSortOrder.setText("Sort order");
		lblFieldSortOrder.setFont(fontBold);

		final var lblFilter = new Label(panFilter, SWT.NONE);
		lblFilter.setText("Filter value");
		lblFilter.setFont(fontBold);

		for (final TableColumnField field : fields) {
			final var lblLabel = new Label(panFilter, SWT.NONE);
			lblLabel.setText(field.getTitle());

			final var gdOperator = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdOperator.widthHint = 130;

			final var cboOperator = new Combo(panFilter, SWT.READ_ONLY);
			cboOperator.setLayoutData(gdOperator);
			cboOperator.setItems(getOperatorsForField(field));
			cboOperator.select(cboOperator.indexOf(getFieldDisplayValue(field, GUITestDataType.SEARCH_OPERATOR)));

			cboOperator.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					updateFilterOperator(field, cboOperator.getItem(cboOperator.getSelectionIndex()));
				}
			});

			final var gdSortOrder = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdSortOrder.widthHint = 80;

			final var cboSortOrder = new Combo(panFilter, SWT.READ_ONLY);
			cboSortOrder.setLayoutData(gdSortOrder);
			cboSortOrder.setItems("", SORT_ORDER_ASC, SORT_ORDER_DESC);
			cboSortOrder.select(cboSortOrder.indexOf(getFieldDisplayValue(field, GUITestDataType.SEARCH_SORT_ORDER)));

			cboSortOrder.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					updateSortOrder(field, cboSortOrder.getText());
				}
			});

			if (field.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				final var gdCboField = new GridData(SWT.LEFT, SWT.CENTER, false, false);
				gdCboField.widthHint = 150;

				final var cboField = new Combo(panFilter, SWT.READ_ONLY);
				cboField.setLayoutData(gdCboField);
				cboField.setItems("", Boolean.TRUE.toString().toLowerCase(), Boolean.FALSE.toString().toLowerCase());
				cboField.select(cboField.indexOf(getFieldDisplayValue(field, GUITestDataType.SEARCH_FILTER)));

				cboField.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						updateFilterData(field, cboField.getText());
					}
				});
			}
			else if (field.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var gdCboField = new GridData(SWT.LEFT, SWT.CENTER, false, false);
				gdCboField.widthHint = 150;

				final var cboField = new Combo(panFilter, SWT.READ_ONLY);
				cboField.setLayoutData(gdCboField);
				cboField.add("");

				cboField.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						updateFilterData(field, cboField.getText());
					}
				});

				// Add all enumeration literals to the combobox
				final var fieldEnum = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();
				fieldEnum.getEnumerationValues().stream().forEach(e -> cboField.add(e.getName()));

				cboField.select(cboField.indexOf(getFieldDisplayValue(field, GUITestDataType.SEARCH_FILTER)));
			}
			else {
				final JavaType type = field.getDTOAttribute().getDomainAttribute().getJavaType();
				final Text txtField;

				if (type.isString() || type.isIntegerOrLong() || type.isUUID()) {
					final var txtInput = new AbstractProposalTextField<String>(panFilter, SWT.BORDER, MIN_FILTER_LENGTH) {
						/*
						 * (non-Javadoc)
						 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang. String)
						 */
						@Override
						public Collection<String> getProposalData(String filter) {
							if (filter.isEmpty())
								return Collections.emptyList();

							return GUITestDataProposalService.searchProposals(filter, field, enableDatabaseLookup);
						}

						/*
						 * (non-Javadoc)
						 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang.
						 * Object)
						 */
						@Override
						public String getProposalLabel(String element) {
							return element;
						}
					};

					txtField = txtInput.getControl();
				}
				else
					txtField = new Text(panFilter, SWT.BORDER);

				txtField.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				txtField.setText(getFieldDisplayValue(field, GUITestDataType.SEARCH_FILTER));

				txtField.addKeyListener(new KeyAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
					 */
					@Override
					public void keyReleased(KeyEvent e) {
						updateFilterData(field, txtField.getText());
					}
				});
			}
		}

		panFilter.pack();
		scrolledComposite.setContent(panFilter);

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);

		if (enableAllActionTypes) {
			createButton(parent, IDialogConstants.DETAILS_ID, "Count", false);
			createButton(parent, IDialogConstants.DESELECT_ALL_ID, "Reset", false);
		}

		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#close()
	 */
	@Override
	public boolean close() {
		if (fontBold != null)
			fontBold.dispose();

		return super.close();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText("Search input dialog");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID)
			selectedActionType = GUITestActionType.ENTER_SEARCH_DATA;
		else if (buttonId == IDialogConstants.DETAILS_ID)
			selectedActionType = GUITestActionType.COUNT_RECORDS;
		else if (buttonId == IDialogConstants.DESELECT_ALL_ID)
			selectedActionType = GUITestActionType.RESET_SEARCH_DATA;

		if (buttonId == IDialogConstants.DETAILS_ID || buttonId == IDialogConstants.DESELECT_ALL_ID)
			okPressed();
		else
			super.buttonPressed(buttonId);
	}

	/**
	 * Update the test data object that belongs to a filter field
	 * @param field
	 * @param value
	 */
	private void updateFilterData(TableColumnField field, String value) {
		updateTestData(field, GUITestDataType.SEARCH_FILTER, value);
	}

	/**
	 * Update the test data object that belongs to a sort order field
	 * @param field
	 * @param value
	 */
	private void updateSortOrder(TableColumnField field, String value) {
		updateTestData(field, GUITestDataType.SEARCH_SORT_ORDER, value);
	}

	/**
	 * Update the test data object that belongs to a filter operator field
	 * @param field
	 * @param value
	 */
	private void updateFilterOperator(TableColumnField field, String value) {
		updateTestData(field, GUITestDataType.SEARCH_OPERATOR, value);
	}

	/**
	 * Update the test data object that is mapped to a corresponding dialog field
	 * @param field
	 * @param type
	 * @param value
	 */
	private void updateTestData(TableColumnField field, GUITestDataType type, String value) {
		// Check if a test data object exists for this field and create it if it cannot be found!
		GUITestData testData = testAction.getTestData().stream()
				.filter(existingTestData -> field.equals(existingTestData.getTableColumnField()) && existingTestData.getType() == type)
				.findFirst().orElse(null);

		if (testData == null) {
			testData = TestingFactory.eINSTANCE.createGUITestData();
			testData.setTableColumnField(field);
			testData.setTestAction(testAction);
			testData.setType(type);
		}

		testData.setNewValue(value);
	}

	/**
	 * @param field
	 * @param type
	 * @return the string to be displayed for a given search field depending on the given test data type
	 */
	private String getFieldDisplayValue(TableColumnField field, GUITestDataType type) {
		return testAction.getTestData().stream().filter(
				testData -> testData.getNewValue() != null && testData.getType() == type && field.equals(testData.getTableColumnField()))
				.findFirst().map(GUITestData::getNewValue).orElse("");
	}

	/**
	 * @param field
	 * @return an array containing all supported operators for a given field
	 */
	private String[] getOperatorsForField(TableColumnField field) {
		final TableColumnFieldTypeEnumeration fieldType = field.getFieldType();

		final var operators = new ArrayList<String>();
		operators.add("");
		operators.add(OPERATOR_EQUAL);

		if (field.hasTemporalType()) {
			operators.add(OPERATOR_GREATER);
			operators.add(OPERATOR_SMALLER);
			operators.add(OPERATOR_GREATER_OR_EQUAL);
			operators.add(OPERATOR_SMALLER_OR_EQUAL);
		}
		else if (fieldType == TableColumnFieldTypeEnumeration.DOUBLE || fieldType == TableColumnFieldTypeEnumeration.FLOAT
				|| fieldType == TableColumnFieldTypeEnumeration.BIG_DECIMAL || fieldType == TableColumnFieldTypeEnumeration.INTEGER
				|| fieldType == TableColumnFieldTypeEnumeration.LONG) {
			operators.add(OPERATOR_GREATER);
			operators.add(OPERATOR_SMALLER);
			operators.add(OPERATOR_GREATER_OR_EQUAL);
			operators.add(OPERATOR_SMALLER_OR_EQUAL);
			operators.add(OPERATOR_IN);
			operators.add(OPERATOR_NOT_IN);

			if (project.hasAngularClient())
				operators.add(OPERATOR_BETWEEN);
		}
		else if (fieldType == TableColumnFieldTypeEnumeration.STRING) {
			operators.add(OPERATOR_LIKE);
			operators.add(OPERATOR_NOT_LIKE);
		}
		else if (fieldType == TableColumnFieldTypeEnumeration.UUID_STRING) {
			operators.add(OPERATOR_LIKE);
			operators.add(OPERATOR_NOT_LIKE);
			operators.add(OPERATOR_IN);
			operators.add(OPERATOR_NOT_IN);
		}
		else if (fieldType == TableColumnFieldTypeEnumeration.UUID_BINARY) {
			operators.add(OPERATOR_IN);
			operators.add(OPERATOR_NOT_IN);
		}

		operators.add(OPERATOR_IS_NULL);
		operators.add(OPERATOR_IS_NOT_NULL);

		return operators.toArray(new String[operators.size()]);
	}

}
