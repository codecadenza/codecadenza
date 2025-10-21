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

import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.TestDataObjectGridPanel;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;

/**
 * <p>
 * Panel for test data attributes that are mapped to objects of a one-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class OneToManyTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private TestDataObjectGridPanel gridTestObjects;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public OneToManyTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		initPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#initPanel()
	 */
	@Override
	protected void initPanel() {
		super.initPanel();

		final MappingObject mappingObject;

		if (testDataAttribute.getMappingAttribute() instanceof final DTOBeanAttribute dtoAttribute)
			mappingObject = dtoAttribute.getReferencedDTOBean();
		else if (testDataAttribute.getMappingAttribute() instanceof final ExchangeMappingAttribute exchangeMappingAttribute)
			mappingObject = (ExchangeMappingObject) exchangeMappingAttribute.getJavaType();
		else
			throw new IllegalStateException("The mapping object could not be determined!");

		final var gdGridTestObjects = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdGridTestObjects.heightHint = 300;

		gridTestObjects = new TestDataObjectGridPanel(this, testModule, testCase, methodInvocation, mappingObject, testDataAttribute,
				validationMode, enableExpectedListSize);
		gridTestObjects.setLayoutData(gdGridTestObjects);
		gridTestObjects.setData(testDataAttribute.getReferencedObjects());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#
	 * validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		gridTestObjects.validateAndApplyInput();
		testDataAttribute.setOperator(getSelectedOperator());
	}

}
