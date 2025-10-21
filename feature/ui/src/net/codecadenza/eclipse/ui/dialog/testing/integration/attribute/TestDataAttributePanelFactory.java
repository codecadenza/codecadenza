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

import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;

/**
 * <p>
 * Factory for creating an {@link AbstractTestDataAttributePanel}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestDataAttributePanelFactory {

	/**
	 * Prevent instantiation
	 */
	private TestDataAttributePanelFactory() {

	}

	/**
	 * Initialize a new panel for maintaining a {@link TestDataAttribute}
	 * @param initializationData the data necessary for the initialization of the panel
	 * @return a new {@link AbstractTestDataAttributePanel} for the given type
	 */
	public static AbstractTestDataAttributePanel initPanel(TestDataAttributePanelData initializationData) {
		final TestDataAttribute testDataAttribute = initializationData.getTestDataAttribute();
		final JavaType type = testDataAttribute.getJavaType();

		if (testDataAttribute.isMappedToElementCollection()) {
			// The validation mode for test data attributes that represent a list is disabled as no operator should be displayed!
			initializationData.setValidationMode(false);

			return new ElementCollectionTestDataAttributePanel(initializationData);
		}
		else if (testDataAttribute.isMappedToFile())
			return new FileChooserTestDataAttributePanel(initializationData);
		else if (type.isIntegerOrLong())
			return new IntegerTestDataAttributePanel(initializationData);
		else if (type.isTemporalType())
			return new DateTestDataAttributePanel(initializationData);
		else if (type.isBoolean())
			return new BooleanTestDataAttributePanel(initializationData);
		else if (type.isDecimalNumber())
			return new DecimalTestDataAttributePanel(initializationData);
		else if (type.isChar())
			return new CharacterTestDataAttributePanel(initializationData);
		else if (type.isUUID())
			return new UUIDTestDataAttributePanel(initializationData);
		else if (type instanceof DTOBean || type instanceof ExchangeMappingObject) {
			if (testDataAttribute.getMappingAttribute().getAssociation() instanceof ManyToOneAssociation)
				return new ManyToOneTestDataAttributePanel(initializationData);
			else if (testDataAttribute.getMappingAttribute().getAssociation() instanceof ManyToManyAssociation) {
				initializationData.setValidationMode(false);
				return new ManyToManyTestDataAttributePanel(initializationData);
			}
			else if (testDataAttribute.isMappedToList()
					|| testDataAttribute.getMappingAttribute().getAssociation() instanceof OneToManyAssociation) {
				initializationData.setValidationMode(false);
				return new OneToManyTestDataAttributePanel(initializationData);
			}
		}
		else if (type instanceof JavaEnum)
			return new EnumTestDataAttributePanel(initializationData);

		return new StringTestDataAttributePanel(initializationData);
	}
}
