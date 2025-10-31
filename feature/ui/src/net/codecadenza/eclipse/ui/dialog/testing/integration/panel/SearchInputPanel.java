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
package net.codecadenza.eclipse.ui.dialog.testing.integration.panel;

import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_FILTER_CRITERIA;
import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_FILTER_OPERATOR;
import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_SEARCH_FIELDS;
import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_SORT_ORDER;

import java.util.ArrayList;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel;
import net.codecadenza.eclipse.ui.dialog.testing.integration.util.AbstractTrackingAttributeDropListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
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
public class SearchInputPanel extends Composite {
	private static final String SORT_ORDER_ASC = "ASC";
	private static final String SORT_ORDER_DESC = "DESC";
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

	private final IntegrationTestModule testModule;
	private final IntegrationTestCase testCase;
	private final IntegrationMethodTestInvocation methodInvocation;
	private final TestDataAttribute searchFieldsAttribute;
	private final boolean addSortOrder;

	/**
	 * Create the panel
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param methodInvocation
	 * @param testDataObject
	 */
	public SearchInputPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation, TestDataObject testDataObject) {
		super(parent, SWT.NONE);

		this.testModule = testModule;
		this.testCase = testCase;
		this.methodInvocation = methodInvocation;
		this.addSortOrder = methodInvocation.getIntegrationMethod().getBoundaryMethod()
				.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH;
		this.searchFieldsAttribute = testDataObject.getAttributeByName(ATTRIBUTE_NAME_SEARCH_FIELDS);

		initPanel();
	}

	/**
	 * Initialize the panel
	 */
	private void initPanel() {
		setLayout(new FillLayout());

		final var glFilter = new GridLayout(addSortOrder ? 4 : 3, false);
		glFilter.horizontalSpacing = 2;
		glFilter.verticalSpacing = 2;

		// Composite for filter criteria
		final var panFilter = new Composite(this, SWT.NONE);
		panFilter.setLayout(glFilter);

		final var lblFieldTitle = new Label(panFilter, SWT.NONE);
		lblFieldTitle.setText("Field");

		final var lblFieldOperator = new Label(panFilter, SWT.NONE);
		lblFieldOperator.setText("Operator");

		if (addSortOrder) {
			final var lblFieldSortOrder = new Label(panFilter, SWT.NONE);
			lblFieldSortOrder.setText("Sort order");
		}

		final var lblFilter = new Label(panFilter, SWT.NONE);
		lblFilter.setText("Filter value");

		for (final TestDataObject searchField : searchFieldsAttribute.getReferencedObjects()) {
			final TestDataAttribute filterAttribute = searchField.getAttributeByName(ATTRIBUTE_NAME_FILTER_CRITERIA);
			final TestDataAttribute operatorAttribute = searchField.getAttributeByName(ATTRIBUTE_NAME_FILTER_OPERATOR);
			final JavaType attributeType = filterAttribute.getJavaType();

			final var lblLabel = new Label(panFilter, SWT.NONE);
			lblLabel.setText(filterAttribute.getLabel());

			final var gdOperator = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdOperator.widthHint = 130;

			final var cboOperator = new Combo(panFilter, SWT.READ_ONLY);
			cboOperator.setLayoutData(gdOperator);
			cboOperator.setItems(getOperatorsForField(filterAttribute));

			cboOperator.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final String selectedOperator = cboOperator.getItem(cboOperator.getSelectionIndex());

					if (selectedOperator.isEmpty())
						operatorAttribute.setValue(null);
					else
						operatorAttribute.setValue(selectedOperator.toUpperCase().replace(' ', '_'));
				}
			});

			if (operatorAttribute.getValue() != null) {
				final String displayedOperator = operatorAttribute.getValue().toLowerCase().replace('_', ' ');

				cboOperator.select(cboOperator.indexOf(displayedOperator));
			}

			if (addSortOrder) {
				final TestDataAttribute sortOrderAttribute = searchField.getAttributeByName(ATTRIBUTE_NAME_SORT_ORDER);

				final var gdSortOrder = new GridData(SWT.LEFT, SWT.CENTER, false, false);
				gdSortOrder.widthHint = 80;

				final var cboSortOrder = new Combo(panFilter, SWT.READ_ONLY);
				cboSortOrder.setLayoutData(gdSortOrder);
				cboSortOrder.setItems("", SORT_ORDER_ASC, SORT_ORDER_DESC);

				cboSortOrder.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						final String selectedSortOrder = cboSortOrder.getItem(cboSortOrder.getSelectionIndex());

						if (selectedSortOrder.isEmpty())
							sortOrderAttribute.setValue(null);
						else
							sortOrderAttribute.setValue(selectedSortOrder);
					}
				});

				if (sortOrderAttribute.getValue() != null)
					cboSortOrder.select(cboSortOrder.indexOf(sortOrderAttribute.getValue()));
			}

			if (attributeType.isBoolean()) {
				final var gdCboField = new GridData(SWT.LEFT, SWT.CENTER, false, false);
				gdCboField.widthHint = 150;

				final var cboField = new Combo(panFilter, SWT.READ_ONLY);
				cboField.setLayoutData(gdCboField);
				cboField.setItems("", Boolean.TRUE.toString().toLowerCase(), Boolean.FALSE.toString().toLowerCase());

				cboField.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						final String selection = cboField.getItem(cboField.getSelectionIndex());

						if (selection.isEmpty())
							filterAttribute.setValue(null);
						else
							filterAttribute.setValue(selection);
					}
				});

				if (filterAttribute.getValue() != null)
					cboField.select(cboField.indexOf(filterAttribute.getValue()));
			}
			else if (attributeType.isEnum()) {
				final var gdCboField = new GridData(SWT.LEFT, SWT.CENTER, false, false);
				gdCboField.widthHint = 150;

				final var cboField = new Combo(panFilter, SWT.READ_ONLY);
				cboField.setLayoutData(gdCboField);
				cboField.add("");

				// Add all enumeration literals to the combobox
				final var fieldEnum = (JavaEnum) attributeType;
				fieldEnum.getEnumerationValues().stream().forEach(e -> cboField.add(e.getName()));

				cboField.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						final String selection = cboField.getItem(cboField.getSelectionIndex());

						if (selection.isEmpty())
							filterAttribute.setValue(null);
						else
							filterAttribute.setValue(selection);
					}
				});

				if (filterAttribute.getValue() != null)
					cboField.select(cboField.indexOf(filterAttribute.getValue()));
			}
			else {
				final Text txtField = new Text(panFilter, SWT.BORDER);
				txtField.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

				txtField.addKeyListener(new KeyAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
					 */
					@Override
					public void keyReleased(KeyEvent e) {
						txtField.setBackground(AbstractTestDataAttributePanel.STANDARD_COLOR);
						filterAttribute.setReferencedAttribute(null);

						if (txtField.getText().isEmpty())
							filterAttribute.setValue(null);
						else
							filterAttribute.setValue(txtField.getText());
					}
				});

				if (filterAttribute.getReferencedAttribute() != null) {
					txtField.setText(AbstractTestDataAttributePanel.TRACKING_PREFIX + filterAttribute.getReferencedAttribute().getId());
					txtField.setBackground(AbstractTestDataAttributePanel.HAS_REFERENCE_COLOR);
				}
				else if (filterAttribute.getValue() != null)
					txtField.setText(filterAttribute.getValue());

				addDropListener(txtField, filterAttribute);
			}
		}
	}

	/**
	 * Add a drop listener to the given text field
	 * @param dropControl the {@link Text} control that should listen for a drop operation
	 * @param testDataAttribute the test data attribute that is mapped to the {@link Text} control
	 */
	private void addDropListener(Text dropControl, TestDataAttribute testDataAttribute) {
		final int operations = DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT;
		final var target = new DropTarget(dropControl, operations);

		new AbstractTrackingAttributeDropListener(target, testModule, testCase, methodInvocation) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.util.AbstractTrackingAttributeDropListener
			 * #onTrackingAttributeReceived(net.codecadenza.eclipse.model.domain.DomainObject,
			 * net.codecadenza.eclipse.model.testing.TestDataAttribute)
			 */
			@Override
			protected void onTrackingAttributeReceived(DomainObject domainObject, TestDataAttribute trackedAttribute) {
				if (testDataAttribute.isReferenceAllowed(domainObject)) {
					testDataAttribute.setReferencedAttribute(trackedAttribute);
					testDataAttribute.setValue(null);

					dropControl.setText(AbstractTestDataAttributePanel.TRACKING_PREFIX + trackedAttribute.getId());
					dropControl.setBackground(AbstractTestDataAttributePanel.HAS_REFERENCE_COLOR);
				}
			}
		};
	}

	/**
	 * @param attribute
	 * @return an array containing all supported operators for a given test data attribute
	 */
	private String[] getOperatorsForField(TestDataAttribute attribute) {
		final JavaType type = attribute.getJavaType();

		final var operators = new ArrayList<String>();
		operators.add("");
		operators.add(OPERATOR_EQUAL);

		if (type.isTemporalType() || type.isNumber()) {
			operators.add(OPERATOR_GREATER);
			operators.add(OPERATOR_SMALLER);
			operators.add(OPERATOR_GREATER_OR_EQUAL);
			operators.add(OPERATOR_SMALLER_OR_EQUAL);
		}
		else if (type.isString() || type.isUUID()) {
			operators.add(OPERATOR_LIKE);
			operators.add(OPERATOR_NOT_LIKE);
		}

		if (type.isString() || type.isUUID() || type.isNumber()) {
			operators.add(OPERATOR_IN);
			operators.add(OPERATOR_NOT_IN);
		}

		if (!type.isPrimitive()) {
			operators.add(OPERATOR_IS_NULL);
			operators.add(OPERATOR_IS_NOT_NULL);
		}

		return operators.toArray(new String[operators.size()]);
	}

}
