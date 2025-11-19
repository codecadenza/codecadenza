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

import java.util.List;
import java.util.StringJoiner;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.shared.Constants;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Panel that contains a tree view for the all invocations of an {@link IntegrationTestCase}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class InvocationTreePanel extends Composite {
	private static final Color EXPECT_TO_FAIL_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
	private static final Color ATTR_TRACKING_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE);
	private static final Color ATTR_REFERENCE_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GREEN);
	private static final String IMG_TOOL_DELETE = org.eclipse.ui.ISharedImages.IMG_TOOL_DELETE;
	private static final String KEY_DOMAIN_OBJECT = "DomainObjectName";
	private static final String KEY_PARENT_INVOCATION_INDEX = "ParentInvocationIndex";
	private static final String KEY_NESTED_INVOCATION_INDEX = "NestedInvocationIndex";

	private final IntegrationTestCase testCase;
	private Tree treeInvocations;
	private Menu mnuTree;
	private MenuItem mniDeleteInvocation;

	/**
	 * Constructor
	 * @param parent
	 * @param testCase
	 */
	public InvocationTreePanel(Composite parent, IntegrationTestCase testCase) {
		super(parent, SWT.NONE);

		this.testCase = testCase;

		initTreePanel();
	}

	/**
	 * Either enable or disable the menu item for deleting an invocation
	 * @param enableDelete
	 */
	public void setEnableDelete(boolean enableDelete) {
		mniDeleteInvocation.setEnabled(enableDelete);
	}

	/**
	 * Callback that is invoked when creating a new nested {@link IntegrationMethodTestInvocation}
	 * @param parentInvocation
	 */
	@SuppressWarnings("unused")
	protected void onCreateNewNestedInvocation(final IntegrationMethodTestInvocation parentInvocation) {
		// Needs to be implemented by a subclass!
	}

	/**
	 * Callback that is invoked when updating a {@link IntegrationMethodTestInvocation}
	 * @param methodInvocation
	 */
	@SuppressWarnings("unused")
	protected void onEditInvocation(final IntegrationMethodTestInvocation methodInvocation) {
		// Needs to be implemented by a subclass!
	}

	/**
	 * @return the selected {@link IntegrationMethodTestInvocation}
	 */
	public IntegrationMethodTestInvocation getSelectedInvocation() {
		final TreeItem[] selItems = treeInvocations.getSelection();
		TreeItem selItem = null;

		for (final TreeItem item : selItems)
			selItem = item;

		if (selItem == null || selItem.getData() == null)
			return null;

		if (selItem.getData() instanceof final IntegrationMethodTestInvocation testInvocation)
			return testInvocation;

		return null;
	}

	/**
	 * @return the selected tracked {@link TestDataAttribute}
	 */
	public TreeItem getSelectedTrackedItem() {
		final TreeItem[] selItems = treeInvocations.getSelection();
		TreeItem selItem = null;

		for (final TreeItem item : selItems)
			selItem = item;

		if (selItem == null || selItem.getData() == null)
			return null;

		if (selItem.getData() instanceof final TestDataAttribute testDataAttribute && testDataAttribute.isTrackValue())
			return selItem;

		return null;
	}

	/**
	 * Refresh the tree that contains the invocations
	 */
	public void refreshTree() {
		treeInvocations.removeAll();

		testCase.getMethodInvocations().forEach(invocation -> {
			final AbstractIntegrationBean integrationBean = invocation.getIntegrationMethod().getIntegrationBean();
			final var integrationBeanService = new IntegrationBeanService(integrationBean.getNamespace().getProject());
			final String methodSignature = integrationBeanService.getMethodSignature(invocation.getIntegrationMethod());

			final var itemInvocation = new TreeItem(treeInvocations, SWT.NONE);
			itemInvocation.setData(invocation);
			itemInvocation.setText(methodSignature);
			itemInvocation.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PUBLIC));

			if (invocation.isExpectToFail())
				itemInvocation.setForeground(EXPECT_TO_FAIL_COLOR);

			addParameterTreeItems(itemInvocation, invocation);
			addReturnValueTreeItem(itemInvocation, invocation);

			for (final IntegrationMethodTestInvocation nestedInvocation : invocation.getNestedInvocations()) {
				final var itemNestedInvocation = new TreeItem(itemInvocation, SWT.NONE);
				itemNestedInvocation.setData(nestedInvocation);
				itemNestedInvocation.setText(methodSignature);
				itemNestedInvocation.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PUBLIC));

				if (invocation.isExpectToFail())
					itemNestedInvocation.setForeground(EXPECT_TO_FAIL_COLOR);

				addParameterTreeItems(itemNestedInvocation, nestedInvocation);
				addReturnValueTreeItem(itemNestedInvocation, nestedInvocation);
			}

			itemInvocation.setExpanded(true);
		});
	}

	/**
	 * Add the parameters to the tree view
	 * @param parentItem the parent tree item
	 * @param testInvocation the test invocation
	 */
	private void addParameterTreeItems(TreeItem parentItem, IntegrationMethodTestInvocation testInvocation) {
		if (testInvocation.getParameters().isEmpty())
			return;

		final var itemParameters = new TreeItem(parentItem, SWT.NONE);
		itemParameters.setText("Parameters");
		itemParameters.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PUBLIC));

		for (final MethodInvocationParameter parameter : testInvocation.getParameters()) {
			final var label = new StringBuilder("Parameter ");
			final JavaType parameterType = parameter.getType();
			final String typeName;
			boolean hasReference = false;

			if (parameterType == null)
				typeName = Constants.INTEGRATION_SEARCH_PARAM_TYPE;
			else
				typeName = parameterType.getName();

			label.append(adaptTypeName(typeName, parameter.isRepresentsList()));
			label.append(" ");
			label.append(parameter.getName());

			if (parameterType != null && !(parameterType instanceof MappingObject)) {
				final TestDataAttribute testDataAttribute = parameter.getParameterValues().getFirst().getAttributes().getFirst();

				if (testDataAttribute.getValue() != null)
					label.append(" = " + testDataAttribute.getValue());
				else if (testDataAttribute.getReferencedAttribute() != null) {
					label.append(" (set by field with ID " + testDataAttribute.getReferencedAttribute().getId() + ")");
					hasReference = true;
				}
			}

			final var itemParameter = new TreeItem(itemParameters, SWT.NONE);
			itemParameter.setText(label.toString());
			itemParameter.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PRIVATE));

			if (hasReference)
				itemParameter.setForeground(ATTR_REFERENCE_COLOR);

			if (parameterType == null || parameterType instanceof MappingObject)
				addTestDataObjects(itemParameter, typeName, parameter.getParameterValues(), testInvocation);
		}
	}

	/**
	 * Add the return value to the tree view
	 * @param parentItem the parent tree item
	 * @param testInvocation the test invocation
	 */
	private void addReturnValueTreeItem(TreeItem parentItem, IntegrationMethodTestInvocation testInvocation) {
		final AbstractIntegrationMethod integrationMethod = testInvocation.getIntegrationMethod();
		final JavaType returnType = integrationMethod.getReturnType();
		final var label = new StringBuilder("Return ");

		if (testInvocation.isReturnVoid() || testInvocation.isExpectedReturnNull() || testInvocation.isExpectToFail())
			return;

		final var itemReturnValue = new TreeItem(parentItem, SWT.NONE);
		itemReturnValue.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PUBLIC));

		if (returnType instanceof final MappingObject mappingObject) {
			label.append(adaptTypeName(mappingObject.getName(), testInvocation.isReturnList()));

			addTestDataObjects(itemReturnValue, mappingObject.getName(), testInvocation.getReturnValues(), testInvocation);
		}
		else if (!testInvocation.getReturnValues().isEmpty()) {
			final TestDataObject returnObject = testInvocation.getReturnValues().getFirst();
			final TestDataAttribute testDataAttribute = returnObject.getAttributes().getFirst();

			label.append(returnType.getName());

			if (testDataAttribute.isTrackValue()) {
				label.append(createTrackingLabel(testDataAttribute.getId()));

				addTrackingData(itemReturnValue, integrationMethod.getIntegrationBean().getDomainObject().getName(), testDataAttribute,
						testInvocation);
			}
			else if (testDataAttribute.getValue() != null)
				label.append(" = " + testDataAttribute.getValue());
		}
		else
			label.append(returnType.getName());

		itemReturnValue.setText(label.toString());
	}

	/**
	 * Add the test data objects to the tree view
	 * @param parentItem the parent tree item
	 * @param typeName the type name of the test data object
	 * @param testDataObjects the list of test data objects to be added
	 * @param testInvocation the method invocation the test data objects belong to
	 */
	private void addTestDataObjects(TreeItem parentItem, String typeName, List<TestDataObject> testDataObjects,
			IntegrationMethodTestInvocation testInvocation) {
		for (final TestDataObject testDataObject : testDataObjects) {
			final var itemTestDataObject = new TreeItem(parentItem, SWT.NONE);
			itemTestDataObject.setText(typeName);
			itemTestDataObject.setData(testDataObject);
			itemTestDataObject.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PRIVATE));

			addTestDataAttributes(itemTestDataObject, testDataObject, testInvocation);
		}
	}

	/**
	 * Add the test data attributes to the tree view
	 * @param parentItem the parent tree item
	 * @param testDataObject the test data that contains the attributes to be added
	 * @param testInvocation the method invocation the attribute belongs to
	 */
	private void addTestDataAttributes(TreeItem parentItem, TestDataObject testDataObject,
			IntegrationMethodTestInvocation testInvocation) {
		for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes()) {
			if (!testDataAttribute.getReferencedObjects().isEmpty()) {
				String typeName;

				final var itemTestDataAttribute = new TreeItem(parentItem, SWT.NONE);
				itemTestDataAttribute.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PRIVATE));

				if (testDataAttribute.getJavaType() != null) {
					typeName = testDataAttribute.getJavaType().getName();

					itemTestDataAttribute.setText(testDataAttribute.getLabel());
				}
				else {
					itemTestDataAttribute.setText(adaptTypeName(Constants.INTEGRATION_SEARCH_FIELD_TYPE, true) + " "
							+ TestDataAttribute.ATTRIBUTE_NAME_SEARCH_FIELDS);

					typeName = Constants.INTEGRATION_SEARCH_FIELD_TYPE;
				}

				addTestDataObjects(itemTestDataAttribute, typeName, testDataAttribute.getReferencedObjects(), testInvocation);
			}
			else {
				final AssertionOperator operator = testDataAttribute.getOperator();
				final String value = testDataAttribute.getValue();
				String assertionLabel = null;

				if (testDataAttribute.isTrackValue())
					assertionLabel = createTrackingLabel(testDataAttribute.getId());
				else if (value != null) {
					if (operator == AssertionOperator.NONE || operator == AssertionOperator.EQUAL)
						assertionLabel = " = '" + value + "'";
					if (operator == AssertionOperator.GREATER)
						assertionLabel = " > '" + value + "'";
					else if (operator == AssertionOperator.GREATER_OR_EQUAL)
						assertionLabel = " >= '" + value + "'";
					else if (operator == AssertionOperator.SMALLER)
						assertionLabel = " < '" + value + "'";
					else if (operator == AssertionOperator.SMALLER_OR_EQUAL)
						assertionLabel = " <= '" + value + "'";
					else if (operator == AssertionOperator.ENDS_WITH)
						assertionLabel = " ends with '" + value + "'";
					else if (operator == AssertionOperator.CONTAINS)
						assertionLabel = " contains '" + value + "'";
					else if (operator == AssertionOperator.STARTS_WITH)
						assertionLabel = " starts with '" + value + "'";
					else if (operator == AssertionOperator.IS_EMPTY)
						assertionLabel = " is empty";
				}
				else if (operator == AssertionOperator.IS_NULL)
					assertionLabel = " is null";
				else if (operator == AssertionOperator.IS_NOT_NULL)
					assertionLabel = " is not null";

				if (assertionLabel != null) {
					final var itemTestDataAttribute = new TreeItem(parentItem, SWT.NONE);
					itemTestDataAttribute.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PRIVATE));
					itemTestDataAttribute.setText(testDataAttribute.getLabel() + assertionLabel);

					if (testDataAttribute.isTrackValue())
						addTrackingData(itemTestDataAttribute, testDataObject.getMappingObject().getDomainObject().getName(),
								testDataAttribute, testInvocation);
				}
				else if (testDataAttribute.getReferencedAttribute() != null) {
					final String referencedId = testDataAttribute.getReferencedAttribute().getId();

					final var itemTestDataAttribute = new TreeItem(parentItem, SWT.NONE);
					itemTestDataAttribute.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_FIELD_PRIVATE));
					itemTestDataAttribute.setText(testDataAttribute.getLabel() + " (set by field with ID " + referencedId + ")");
					itemTestDataAttribute.setForeground(ATTR_REFERENCE_COLOR);
				}
			}
		}
	}

	/**
	 * Add data to the given {@link TreeItem} so that a drop operation is able to extract information for creating a reference on
	 * the given tracked {@link TestDataAttribute}
	 * @param treeItem
	 * @param domainObjectName
	 * @param testDataAttribute
	 * @param testInvocation
	 */
	private void addTrackingData(TreeItem treeItem, String domainObjectName, TestDataAttribute testDataAttribute,
			IntegrationMethodTestInvocation testInvocation) {
		treeItem.setData(testDataAttribute);
		treeItem.setData(KEY_DOMAIN_OBJECT, domainObjectName);
		treeItem.setForeground(ATTR_TRACKING_COLOR);

		if (testInvocation.getParentInvocation() == null) {
			treeItem.setData(KEY_PARENT_INVOCATION_INDEX, testCase.getMethodInvocations().indexOf(testInvocation));
			treeItem.setData(KEY_NESTED_INVOCATION_INDEX, -1);
		}
		else {
			final int parentIndex = testCase.getMethodInvocations().indexOf(testInvocation.getParentInvocation());
			final IntegrationMethodTestInvocation parentInvocation = testInvocation.getParentInvocation();

			treeItem.setData(KEY_PARENT_INVOCATION_INDEX, parentIndex);
			treeItem.setData(KEY_NESTED_INVOCATION_INDEX, parentInvocation.getNestedInvocations().indexOf(testInvocation));
		}
	}

	/**
	 * Adapt the given type name if it represents a list
	 * @param typeName
	 * @param isList
	 * @return the adapted name
	 */
	private String adaptTypeName(String typeName, boolean isList) {
		if (!isList)
			return typeName;

		return "List<" + typeName + ">";
	}

	/**
	 * Create the part of the label for displaying the ID of a tracked attribute
	 * @param id
	 * @return the generated label
	 */
	private String createTrackingLabel(String id) {
		return " (tracked - using ID " + id + ")";
	}

	/**
	 * Initialize the panel
	 */
	private void initTreePanel() {
		setLayout(new FillLayout());

		final var groupTree = new Group(this, SWT.NONE);
		groupTree.setLayout(new GridLayout());
		groupTree.setText("Existing invocations");

		treeInvocations = new Tree(groupTree, SWT.BORDER);
		treeInvocations.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeInvocations.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				final IntegrationMethodTestInvocation selInvocation = getSelectedInvocation();

				if (selInvocation == null || !mnuTree.getEnabled())
					return;

				onEditInvocation(selInvocation);
			}
		});

		treeInvocations.addMenuDetectListener(e -> {
			treeInvocations.setMenu(null);

			if (getSelectedInvocation() != null)
				treeInvocations.setMenu(mnuTree);
		});

		// Provide the data in text format
		final TextTransfer textTransfer = TextTransfer.getInstance();
		final var transferTypes = new Transfer[] { textTransfer };

		final var dragSource = new DragSource(treeInvocations, DND.DROP_MOVE | DND.DROP_COPY);
		dragSource.setTransfer(transferTypes);

		dragSource.addDragListener(new DragSourceAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				final TreeItem trackedItem = getSelectedTrackedItem();

				if (trackedItem == null) {
					event.doit = false;
					return;
				}

				event.doit = true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				final TreeItem trackedItem = getSelectedTrackedItem();

				if (trackedItem == null)
					return;

				final TestDataAttribute trackedAttribute = (TestDataAttribute) trackedItem.getData();
				final String domainObjectName = trackedItem.getData(KEY_DOMAIN_OBJECT).toString();
				final MappingAttribute mappingAttribute = trackedAttribute.getMappingAttribute();
				final String attributeName = mappingAttribute != null ? mappingAttribute.getDomainAttribute().getName() : "";
				final String parentInvocationIndex = trackedItem.getData(KEY_PARENT_INVOCATION_INDEX).toString();
				final String nestedInvocationIndex = trackedItem.getData(KEY_NESTED_INVOCATION_INDEX).toString();

				final StringJoiner joiner = new StringJoiner(" ");
				joiner.add(domainObjectName);
				joiner.add(attributeName);
				joiner.add(trackedAttribute.getId());
				joiner.add(parentInvocationIndex);
				joiner.add(nestedInvocationIndex);

				event.data = joiner.toString();
			}
		});

		mnuTree = new Menu(treeInvocations);

		final var mniAddNestedInvocation = new MenuItem(mnuTree, SWT.NONE);
		mniAddNestedInvocation.setText("Add");

		mniAddNestedInvocation.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final IntegrationMethodTestInvocation selInvocation = getSelectedInvocation();

				if (selInvocation == null)
					return;

				if (selInvocation.getParentInvocation() != null) {
					MessageDialog.openConfirm(Display.getCurrent().getActiveShell(), "Add nested invocation",
							"A nested invocation can only be added to a top-level invocation!");
					return;
				}

				onCreateNewNestedInvocation(selInvocation);
			}
		});

		final var mniEditInvocation = new MenuItem(mnuTree, SWT.NONE);
		mniEditInvocation.setText("Edit");

		mniEditInvocation.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final IntegrationMethodTestInvocation selInvocation = getSelectedInvocation();

				if (selInvocation == null)
					return;

				onEditInvocation(selInvocation);
			}
		});

		mniDeleteInvocation = new MenuItem(mnuTree, SWT.NONE);
		mniDeleteInvocation.setText("Delete");
		mniDeleteInvocation.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(IMG_TOOL_DELETE));

		mniDeleteInvocation.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final IntegrationMethodTestInvocation selInvocation = getSelectedInvocation();

				if (selInvocation == null)
					return;

				boolean deleteAllowed = isInvocationAllowedToBeDeleted(selInvocation);

				if (deleteAllowed)
					deleteAllowed = selInvocation.getNestedInvocations().stream().allMatch(n -> isInvocationAllowedToBeDeleted(n));

				if (!deleteAllowed) {
					MessageDialog.openInformation(Display.getCurrent().getActiveShell(), "Delete",
							"Cannot delete the selected method invocation. Remove all references first!");
					return;
				}

				if (!MessageDialog.openConfirm(Display.getCurrent().getActiveShell(), "Delete",
						"Do you really want to delete the selected method invocation?"))
					return;

				if (selInvocation.getParentInvocation() == null)
					testCase.getMethodInvocations().remove(selInvocation);
				else
					selInvocation.getParentInvocation().getNestedInvocations().remove(selInvocation);

				refreshTree();
			}
		});
	}

	/**
	 * Check if the given {@link IntegrationMethodTestInvocation} contains attributes that are referenced by other method
	 * invocations
	 * @param invocationToDelete
	 * @return true if the given {@link IntegrationMethodTestInvocation} can be deleted
	 */
	private boolean isInvocationAllowedToBeDeleted(IntegrationMethodTestInvocation invocationToDelete) {
		final List<TestDataAttribute> attributesToDelete = invocationToDelete.getAllAttributes();

		for (final IntegrationMethodTestInvocation invocation : testCase.getMethodInvocations()) {
			for (final IntegrationMethodTestInvocation nestedInvocation : invocation.getNestedInvocations()) {
				if (invocationToDelete.equals(nestedInvocation))
					continue;

				final boolean nestedReferenceFound = nestedInvocation.getAllAttributes().stream()
						.filter(a -> a.getReferencedAttribute() != null)
						.anyMatch(a -> attributesToDelete.contains(a.getReferencedAttribute()));

				if (nestedReferenceFound)
					return false;
			}

			if (invocationToDelete.equals(invocation))
				continue;

			final boolean referenceFound = invocation.getAllAttributes().stream().filter(a -> a.getReferencedAttribute() != null)
					.anyMatch(a -> attributesToDelete.contains(a.getReferencedAttribute()));

			if (referenceFound)
				return false;
		}

		return true;
	}

}
