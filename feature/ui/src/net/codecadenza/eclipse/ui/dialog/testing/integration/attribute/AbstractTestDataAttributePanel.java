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
package net.codecadenza.eclipse.ui.dialog.testing.integration.attribute;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.ui.dialog.testing.integration.util.AbstractTrackingAttributeDropListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;

/**
 * <p>
 * Panel for test data attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTestDataAttributePanel extends Composite {
	public static final String TRACKING_PREFIX = "Set by tracking of attribute ";
	public static final Color HAS_REFERENCE_COLOR = new Color(Display.getCurrent(), 230, 225, 225);
	public static final Color READONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);
	public static final Color STANDARD_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);

	protected final Project project;
	protected final IntegrationTestModule testModule;
	protected final IntegrationTestCase testCase;
	protected final IntegrationMethodTestInvocation methodInvocation;
	protected final TestDataAttribute testDataAttribute;
	protected final boolean validationMode;
	protected boolean enableExpectedListSize;
	protected Combo cboOperator;

	/**
	 * Constructor
	 * @param initializationData
	 */
	protected AbstractTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData.getParent(), SWT.NONE);

		this.project = initializationData.getTestModule().getProject();
		this.testModule = initializationData.getTestModule();
		this.testCase = initializationData.getTestCase();
		this.methodInvocation = initializationData.getMethodInvocation();
		this.testDataAttribute = initializationData.getTestDataAttribute();
		this.validationMode = initializationData.isValidationMode();
		this.enableExpectedListSize = initializationData.isEnableExpectedListSize();
	}

	/**
	 * Validate the input and apply it
	 */
	public abstract void validateAndApplyInput();

	/**
	 * A subclass can define what should happen if the selected operator has been changed
	 */
	protected void onSelectedOperatorChanged() {

	}

	/**
	 * A subclass can define how to set a reference on the given attribute
	 * @param domainObject the domain object
	 * @param trackedAttribute the tracked attribute
	 */
	@SuppressWarnings("unused")
	protected void onSetReference(DomainObject domainObject, TestDataAttribute trackedAttribute) {

	}

	/**
	 * A subclass can define how to set a random value
	 */
	public void onRequestRandomValue() {

	}

	/**
	 * Set the layout
	 */
	protected void initPanel() {
		final GridLayout glMain = createLayout();

		if (validationMode) {
			final var gdOperator = new GridData(SWT.FILL, SWT.TOP, false, false);
			gdOperator.widthHint = 150;

			cboOperator = new Combo(this, SWT.READ_ONLY);
			cboOperator.setLayoutData(gdOperator);

			for (final String operatorName : getOperatorNamesByType(testDataAttribute.getJavaType()))
				cboOperator.add(operatorName);

			int itemIndex = 0;

			for (final String item : cboOperator.getItems()) {
				if (item.equals(testDataAttribute.getOperator().name())) {
					cboOperator.select(itemIndex);
					break;
				}

				itemIndex++;
			}

			cboOperator.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					onSelectedOperatorChanged();
				}
			});
		}

		setLayout(glMain);
	}

	/**
	 * @return the {@link GridLayout} used by the panel
	 */
	protected GridLayout createLayout() {
		final int cols = validationMode ? 2 : 1;

		final GridLayout glMain = new GridLayout(cols, false);
		glMain.marginWidth = 0;
		glMain.marginHeight = 0;

		return glMain;
	}

	/**
	 * @return the selected assertion operator
	 */
	protected AssertionOperator getSelectedOperator() {
		if (cboOperator == null)
			return AssertionOperator.NONE;

		return AssertionOperator.valueOf(cboOperator.getItem(cboOperator.getSelectionIndex()));
	}

	/**
	 * @return true if the input field is editable
	 */
	protected boolean isEditable() {
		return !(getSelectedOperator() == AssertionOperator.IS_NULL || getSelectedOperator() == AssertionOperator.IS_NOT_NULL
				|| getSelectedOperator() == AssertionOperator.IS_EMPTY);
	}

	/**
	 * Set the background color of the given control depending on the state of the {@link TestDataAttribute}
	 * @param control
	 */
	protected void setBackgroundColor(Control control) {
		if (testDataAttribute.getReferencedAttribute() != null)
			control.setBackground(HAS_REFERENCE_COLOR);
		else if (!isEditable())
			control.setBackground(READONLY_COLOR);
		else
			control.setBackground(STANDARD_COLOR);
	}

	/**
	 * Get the names of the supported operators for a given type
	 * @param type
	 * @return the list with supported operator names
	 */
	private List<String> getOperatorNamesByType(JavaType type) {
		final List<AssertionOperator> operators = new ArrayList<>();
		operators.add(AssertionOperator.NONE);
		operators.add(AssertionOperator.EQUAL);

		if (type.isNumber() || type.isTemporalType()) {
			operators.add(AssertionOperator.GREATER);
			operators.add(AssertionOperator.GREATER_OR_EQUAL);
			operators.add(AssertionOperator.SMALLER);
			operators.add(AssertionOperator.SMALLER_OR_EQUAL);
		}
		else if (type.isString()) {
			operators.add(AssertionOperator.CONTAINS);
			operators.add(AssertionOperator.STARTS_WITH);
			operators.add(AssertionOperator.ENDS_WITH);
			operators.add(AssertionOperator.IS_EMPTY);
		}

		if ((type instanceof DTOBean || type instanceof ExchangeMappingObject)
				&& (testDataAttribute.getMappingAttribute().getAssociation() instanceof OneToManyAssociation
						|| testDataAttribute.getMappingAttribute().getAssociation() instanceof ManyToManyAssociation))
			operators.add(AssertionOperator.CONTAINS);
		else if (!type.isPrimitive()) {
			operators.add(AssertionOperator.IS_NULL);
			operators.add(AssertionOperator.IS_NOT_NULL);
		}

		return operators.stream().map(AssertionOperator::name).toList();
	}

	/**
	 * Initialize the panel to handle drop operations
	 * @param dropControl the control that should listen for a drop operation
	 */
	protected void initDropListener(Control dropControl) {
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
				onSetReference(domainObject, trackedAttribute);
			}
		};
	}

}
