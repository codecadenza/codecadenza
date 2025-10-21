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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import net.codecadenza.eclipse.ui.dialog.testing.integration.EditTestObjectDialog;
import net.codecadenza.eclipse.ui.dialog.testing.integration.util.AbstractTrackingAttributeDropListener;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Grid for test data objects for either the parameters or the return value of a {@link IntegrationMethodTestInvocation}. It can
 * also be used for the referenced test data objects of a {@link TestDataAttribute}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestDataObjectGridPanel extends Composite {
	private final IntegrationTestModule testModule;
	private final IntegrationTestCase testCase;
	private final MappingObject mappingObject;
	private final Map<Integer, MappingAttribute> columnMap = new HashMap<>();
	private final IntegrationMethodTestInvocation methodInvocation;
	private final boolean validationMode;
	private TestDataAttribute testDataAttribute;
	private Combo cboOperator;
	private Text txtExpectedSize;
	private DataGridComposite<TestDataObject> dataGrid;

	/**
	 * Constructor for handling either parameters or return values of a method invocation
	 * @param parent the parent component
	 * @param testModule the test module
	 * @param testCase the integration test case
	 * @param mappingObject the {@link MappingObject} that is used as a template for creating the grid columns
	 * @param methodInvocation the method invocation
	 * @param validationMode flag that controls if data should either by entered or validated
	 * @param enableExpectedListSize flag that controls if the number of returned objects should be validated
	 */
	public TestDataObjectGridPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			MappingObject mappingObject, IntegrationMethodTestInvocation methodInvocation, boolean validationMode,
			boolean enableExpectedListSize) {
		this(parent, testModule, testCase, methodInvocation, mappingObject, validationMode);

		if (enableExpectedListSize)
			initExpectedSizePanel();

		initDataGrid();
	}

	/**
	 * Constructor for handling test data attributes that are mapped to list
	 * @param parent the parent component
	 * @param testModule the test module
	 * @param testCase the integration test case
	 * @param methodInvocation the method invocation
	 * @param mappingObject the {@link MappingObject} that is used as a template for creating the grid columns
	 * @param testDataAttribute the test data attribute
	 * @param validationMode flag that controls if data should either by entered or validated
	 * @param enableExpectedListSize flag that controls if the number of returned objects should be validated
	 */
	public TestDataObjectGridPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation, MappingObject mappingObject, TestDataAttribute testDataAttribute,
			boolean validationMode, boolean enableExpectedListSize) {
		this(parent, testModule, testCase, methodInvocation, mappingObject, validationMode);

		this.testDataAttribute = testDataAttribute;

		if (enableExpectedListSize)
			initExpectedSizePanel();

		initDataGrid();

	}

	/**
	 * Constructor
	 * @param parent the parent component
	 * @param testModule the test module
	 * @param testCase the integration test case
	 * @param methodInvocation the method invocation
	 * @param mappingObject the {@link MappingObject} that is used as a template for creating the grid columns
	 * @param validationMode flag that controls if data is either entered or validated
	 */
	private TestDataObjectGridPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation, MappingObject mappingObject, boolean validationMode) {
		super(parent, SWT.NONE);

		this.setLayout(new GridLayout());
		this.testModule = testModule;
		this.testCase = testCase;
		this.methodInvocation = methodInvocation;
		this.mappingObject = mappingObject;
		this.validationMode = validationMode;
	}

	/**
	 * Initialize the panel for the expected size
	 */
	private void initExpectedSizePanel() {
		final var panExpectedSize = new Composite(this, SWT.NONE);
		panExpectedSize.setLayout(new GridLayout(3, false));
		panExpectedSize.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblLabel = new Label(panExpectedSize, SWT.NONE);
		lblLabel.setText("Expected number of returned objects");

		final var gdOperator = new GridData(SWT.LEFT, SWT.CENTER, false, false);
		gdOperator.widthHint = 150;

		cboOperator = new Combo(panExpectedSize, SWT.READ_ONLY);
		cboOperator.setLayoutData(gdOperator);
		cboOperator.add(AssertionOperator.NONE.name());
		cboOperator.add(AssertionOperator.EQUAL.name());
		cboOperator.add(AssertionOperator.GREATER.name());
		cboOperator.add(AssertionOperator.GREATER_OR_EQUAL.name());
		cboOperator.add(AssertionOperator.SMALLER.name());
		cboOperator.add(AssertionOperator.SMALLER_OR_EQUAL.name());
		cboOperator.select(0);

		if (testDataAttribute != null)
			cboOperator.select(cboOperator.indexOf(testDataAttribute.getExpectedSizeOperator().name()));
		else
			cboOperator.select(cboOperator.indexOf(methodInvocation.getExpectedSizeOperator().name()));

		txtExpectedSize = new Text(panExpectedSize, SWT.BORDER);
		txtExpectedSize.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		if (testDataAttribute != null && testDataAttribute.getExpectedSize() != null)
			txtExpectedSize.setText(Integer.toString(testDataAttribute.getExpectedSize()));
		else if (methodInvocation.getExpectedSize() != null)
			txtExpectedSize.setText(Integer.toString(methodInvocation.getExpectedSize()));
	}

	/**
	 * Validate the input and apply it
	 */
	public void validateAndApplyInput() {
		if (txtExpectedSize == null)
			return;

		final String selectedOperatorName = cboOperator.getItem(cboOperator.getSelectionIndex());

		if (!txtExpectedSize.getText().isEmpty()) {
			try {
				final int expectedSize = Integer.parseInt(txtExpectedSize.getText());

				if (testDataAttribute != null) {
					testDataAttribute.setExpectedSize(expectedSize);

					if (testDataAttribute.getId() == null)
						testDataAttribute.setId(UUID.randomUUID().toString());
				}
				else
					methodInvocation.setExpectedSize(expectedSize);
			}
			catch (final NumberFormatException e) {
				txtExpectedSize.setFocus();
				throw e;
			}
		}
		else if (testDataAttribute != null) {
			testDataAttribute.setExpectedSize(null);
			testDataAttribute.setId(null);
		}
		else
			methodInvocation.setExpectedSize(null);

		if (testDataAttribute != null)
			testDataAttribute.setExpectedSizeOperator(AssertionOperator.valueOf(selectedOperatorName));
		else
			methodInvocation.setExpectedSizeOperator(AssertionOperator.valueOf(selectedOperatorName));
	}

	/**
	 * Set the data to be displayed in the data grid
	 * @param testDataObjects
	 */
	public void setData(List<TestDataObject> testDataObjects) {
		dataGrid.setData(testDataObjects);
	}

	/**
	 * Initialize the grid that contains the test objects
	 */
	private void initDataGrid() {
		int columnIndex = 0;

		dataGrid = new DataGridComposite<>(this, SWT.BORDER, false) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(TestDataObject testDataObject) {
				if (testDataObject == null)
					return;

				final var dlg = new EditTestObjectDialog(getShell(), testModule, testCase, methodInvocation, testDataObject,
						validationMode);

				if (dlg.open() == Dialog.OK)
					refresh();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(TestDataObject element, int columnIndex) {
				final MappingAttribute mappingAttribute = columnMap.get(columnIndex);

				for (final TestDataAttribute attribute : element.getAttributes())
					if (attribute.getMappingAttribute().equals(mappingAttribute)) {
						if (attribute.getReferencedAttribute() != null)
							return "Set by tracking of attribute " + attribute.getReferencedAttribute().getId();

						return attribute.getValue();
					}

				return null;
			}
		};

		dataGrid.getTableViewer().getTable().addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyPressed(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.keyCode == SWT.KEYPAD_ADD)
					addObject(null, null);
				else if (event.keyCode == SWT.DEL)
					removeSelectedTestObjects();
			}
		});

		if (mappingObject instanceof final DTOBean dto)
			for (final DTOBeanAttribute dtoAttribute : dto.getAttributes())
				columnIndex = addColumn(columnIndex, dtoAttribute);
		else if (mappingObject instanceof final ExchangeMappingObject exchangeMappingObject)
			for (final ExchangeMappingAttribute exchangeMappingAttribute : exchangeMappingObject.getAttributes())
				columnIndex = addColumn(columnIndex, exchangeMappingAttribute);

		dataGrid.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		addMenu();
		addDropListener();
	}

	/**
	 * Create the menu
	 */
	private void addMenu() {
		final var mniAddObject = new MenuItem(dataGrid.getPopUpMenu(), SWT.NONE);
		mniAddObject.setText("Add object");

		mniAddObject.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addObject(null, null);
			}
		});

		final var mniRemoveSelectedTestObject = new MenuItem(dataGrid.getPopUpMenu(), SWT.NONE);
		mniRemoveSelectedTestObject.setText("Remove object");

		mniRemoveSelectedTestObject.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				removeSelectedTestObjects();
			}
		});
	}

	/**
	 * Open a dialog to add a new test data object
	 * @param domainObject
	 * @param trackedAttribute
	 */
	private void addObject(DomainObject domainObject, TestDataAttribute trackedAttribute) {
		final TestDataObject testDataObject = new IntegrationTestCaseService(testModule).initTestObject(mappingObject,
				methodInvocation.getIntegrationMethod().getBoundaryMethod(), false, false);

		if (trackedAttribute != null && domainObject != null) {
			// Do not add a new object if the reference is not allowed to be set!
			if (testDataObject.getPKAttribute().isReferenceAllowed(domainObject))
				testDataObject.getPKAttribute().setReferencedAttribute(trackedAttribute);
			else
				return;
		}

		final var dlg = new EditTestObjectDialog(getShell(), testModule, testCase, methodInvocation, testDataObject, validationMode);

		if (dlg.open() == Dialog.OK) {
			dataGrid.getData().add(testDataObject);
			dataGrid.refresh();
		}
	}

	/**
	 * Remove the selected test data objects
	 */
	private void removeSelectedTestObjects() {
		for (final TestDataObject selectedObject : dataGrid.getAllSelectedElements())
			dataGrid.getData().remove(selectedObject);

		dataGrid.refresh();
	}

	/**
	 * Add a column for the given {@link MappingAttribute} if the attribute isn't mapped to a complex type (e.g. a DTOBean)
	 * @param columnIndex the column index
	 * @param attribute the mapping attribute
	 * @return the next column index if a new column has been added
	 */
	private int addColumn(int columnIndex, MappingAttribute attribute) {
		if (attribute.getDomainAttribute() == null)
			return columnIndex;

		dataGrid.addColumn(attribute.getName());
		columnMap.put(columnIndex++, attribute);

		return columnIndex;
	}

	/**
	 * Add a drop listener to the given text field
	 */
	private void addDropListener() {
		final int operations = DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT;
		final var target = new DropTarget(dataGrid, operations);

		new AbstractTrackingAttributeDropListener(target, testModule, testCase, methodInvocation) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.util.AbstractTrackingAttributeDropListener
			 * #onTrackingAttributeReceived(net.codecadenza.eclipse.model.domain.DomainObject,
			 * net.codecadenza.eclipse.model.testing.TestDataAttribute)
			 */
			@Override
			protected void onTrackingAttributeReceived(DomainObject domainObject, TestDataAttribute trackedAttribute) {
				// Add a new object initialized with the primary key attribute that references the given tracking attribute
				addObject(domainObject, trackedAttribute);
			}
		};
	}
}
