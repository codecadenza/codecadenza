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
package net.codecadenza.runtime.validation;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.codecadenza.runtime.validation.util.ConstrainedClass;
import net.codecadenza.runtime.validation.util.ConstraintViolation;
import net.codecadenza.runtime.validation.util.validator.ConstraintValidator;
import net.codecadenza.runtime.validation.util.validator.ValidatorFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * This class provides validation services within the constraint framework
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BeanValidator {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final ConstrainedClass constrainedClass;
	private final Object validationTarget;

	/**
	 * Constructor
	 * @param validationTarget
	 * @throws IllegalArgumentException if the validationTarget is null
	 */
	public BeanValidator(Object validationTarget) {
		if (validationTarget == null)
			throw new IllegalArgumentException("NULL is not a legal value for validationTarget");

		this.validationTarget = validationTarget;

		constrainedClass = ConstrainedClass.getConstrainedClass(validationTarget.getClass());
	}

	/**
	 * @return the {@link ConstrainedClass} that provides access to the property constraints for this class
	 */
	public ConstrainedClass getConstrainedClass() {
		return constrainedClass;
	}

	/**
	 * Validate the object
	 * @throws PropertyConstraintViolationException if the validation of a property value has failed
	 */
	public void validate() {
		final Set<String> properties = constrainedClass.getConstrainedProperties();
		final var b = new StringBuilder();
		boolean isFirstViolation = true;

		for (final String propertyName : properties) {
			try {
				final List<ConstraintViolation> violations = validateProperty(constrainedClass, propertyName, getValue(propertyName));

				for (final ConstraintViolation v : violations) {
					if (isFirstViolation)
						isFirstViolation = false;
					else
						b.append("\n");

					b.append(v.getMessage());
				}
			}
			catch (final IntrospectionException e) {
				logger.error("Error while performing introspection of property '{}'!", propertyName, e);
			}
		}

		if (b.length() != 0)
			throw new PropertyConstraintViolationException(b.toString());
	}

	/**
	 * Checks if the value is valid
	 * @param constrainedClass
	 * @param propertyName
	 * @param value
	 * @return a list of constraint violations
	 * @throws IntrospectionException if there are problems accessing the property
	 */
	public List<ConstraintViolation> validateProperty(ConstrainedClass constrainedClass, String propertyName, Object value)
			throws IntrospectionException {
		if (constrainedClass == null)
			throw new IllegalArgumentException("null is not a legal value for constrainedClass");

		if (propertyName == null)
			throw new IllegalArgumentException("null is not a legal value for propertyName");

		// First check for constraints that this class has placed on this property
		final Set<Annotation> constraints = constrainedClass.getConstraints(propertyName);

		return checkConstraints(value, constraints);
	}

	/**
	 * Check the constraints
	 * @param value the value to be checked
	 * @param annotations
	 * @return a list of constraint violations
	 */
	public static List<ConstraintViolation> checkConstraints(Object value, Set<Annotation> annotations) {
		final var violations = new ArrayList<ConstraintViolation>();

		annotations.forEach(a -> {
			final ConstraintValidator v = ValidatorFactory.getInstance().getValidatorFor(a.annotationType());

			if (v != null) {
				final ConstraintViolation violation = v.validate(value, a);

				if (violation != null)
					violations.add(violation);
			}
		});

		return violations;
	}

	/**
	 * Get the property value
	 * @param propertyName the name of the property
	 * @return the value
	 * @throws IntrospectionException if the introspection has failed
	 */
	private Object getValue(String propertyName) throws IntrospectionException {
		// Get the property value via the getter method
		try {
			final PropertyDescriptor[] descs = Introspector.getBeanInfo(validationTarget.getClass()).getPropertyDescriptors();

			for (final PropertyDescriptor desc : descs) {
				if (desc.getName().equals(propertyName)) {
					final Method readMethod = desc.getReadMethod();

					if (readMethod == null)
						break;

					return readMethod.invoke(validationTarget);
				}
			}
		}
		catch (final IllegalAccessException | InvocationTargetException e) {
			throw new IntrospectionException("Error while invoking getter method for property '" + propertyName + "'!");
		}

		throw new IntrospectionException("The getter method for property '" + propertyName + "' could not be found!");
	}

}
