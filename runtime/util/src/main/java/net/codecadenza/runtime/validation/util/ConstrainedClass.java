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
package net.codecadenza.runtime.validation.util;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import net.codecadenza.runtime.validation.Constraint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * A class to provide access to annotation-based property constraints. ConstrainedClass supports constraints that are added to the
 * getters and setters of JavaBean properties. It does not support constraints added to either the field of the property or the
 * method parameter of the setter.
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ConstrainedClass {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static Map<Class<?>, ConstrainedClass> constrainedClasses = Collections.synchronizedMap(new WeakHashMap<>());

	private Map<String, Set<Annotation>> propertyConstraintMap;
	private Set<String> constrainedProperties;
	private final Class<?> thisClass;

	/**
	 * Prevent instantiation
	 * @param c
	 */
	private ConstrainedClass(Class<?> c) {
		thisClass = c;
		buildConstraintMap();
	}

	/**
	 * Get the {@link ConstrainedClass} for the given class
	 * @param theClass the class to be wrapped
	 * @return the ConstrainedClass that wraps the given class
	 * @throws IllegalArgumentException if the provided class is null
	 */
	public static ConstrainedClass getConstrainedClass(Class<?> theClass) {
		/*
		 * We make an effort to avoid the same ConstrainedClass being created more than once (i.e. on different threads) as there is
		 * potentially a fair bit of reflection that goes on during construction. The technique we use is to take a lock on the class
		 * (e.g. Address.class) and hold it while we create the associated ConstrainedClass. Note that this looks very much like the
		 * double checked lock trick, which is known to be broken. However, in this case the class state that is being changed are
		 * entries in a synchronized map. If we removed the "synchronized (theClass)" statement, the code would still be thread safe,
		 * but we could get cases where we were creating unnecessary copies of ConstrainedClass objects. As the process of
		 * constructing an instance of ConstrainedClass for a given class should always yield the same result, this is not a problem
		 * other than, potentially, a temporary waste of CPU and memory.
		 */
		ConstrainedClass cc = constrainedClasses.get(theClass);

		if (cc == null)
			synchronized (theClass) {
				// Now that we have the lock on the underlying class. We just do a quick check that none got in ahead of us.
				cc = constrainedClasses.computeIfAbsent(theClass, ConstrainedClass::new);
			}

		return cc;
	}

	/**
	 * @param annotation the annotation to test
	 * @return true if the given annotation is a Constraint
	 * @throws IllegalArgumentException if the annotation is null
	 */
	public static boolean isConstraint(Annotation annotation) {
		if (annotation == null)
			throw new IllegalArgumentException("null is not a legal value for annotation");

		return annotation.annotationType().isAnnotationPresent(Constraint.class);
	}

	/**
	 * Get an immutable set of constraints for a given property. The constraints may be annotated either on the getter or the setter
	 * method and may be anywhere within the class hierarchy. The result is a set of constraints that must all be satisfied for the
	 * property to be valid.
	 * @param propertyName name of the property
	 * @return the set (immutable) of constraints (annotations) or an empty set if there are no constraints on that property
	 * @throws IntrospectionException if there are problems accessing the property
	 * @throws IllegalArgumentException if the propertyName is null
	 */
	public Set<Annotation> getConstraints(String propertyName) throws IntrospectionException {
		if (propertyName == null)
			throw new IllegalArgumentException("null is not a legal value for propertyName");

		final Set<Annotation> c = propertyConstraintMap.get(propertyName);

		if (c == null)
			throw new IntrospectionException("propertyName");

		return c;
	}

	/**
	 * @return a set of properties that have constraints associated with them
	 */
	public Set<String> getConstrainedProperties() {
		return constrainedProperties;
	}

	/**
	 * Build the constraint map
	 */
	private void buildConstraintMap() {
		final var m = new HashMap<String, Set<Annotation>>();
		final var s = new HashSet<String>();

		try {
			final PropertyDescriptor[] descs = Introspector.getBeanInfo(thisClass).getPropertyDescriptors();

			for (final PropertyDescriptor desc : descs) {
				final String propName = desc.getName();
				Set<Annotation> constraints = new HashSet<>();

				// Add all constraints that are on the getter method
				final Method read = desc.getReadMethod();

				if (read != null)
					addConstraints(constraints, thisClass, read.getName(), read.getParameterTypes());

				// Add all constraints that are on the setter method. Note that these will take precedence for the same constraint types
				// that were on the setter.
				final Method write = desc.getWriteMethod();

				if (write != null)
					addConstraints(constraints, thisClass, write.getName(), write.getParameterTypes());

				// This is a fixed set of constraints so we turn it into an unmodifiable set
				if (!constraints.isEmpty()) {
					constraints = Collections.unmodifiableSet(constraints);
					s.add(propName);
				}
				else
					constraints = Collections.emptySet();

				m.put(propName, constraints);
			}
		}
		catch (final IntrospectionException e) {
			logger.error("Error while building constraints!", e);
		}

		// This is a fixed map of properties with constraints so we turn it into an unmodifiable map
		if (!m.isEmpty())
			propertyConstraintMap = Collections.unmodifiableMap(m);
		else
			propertyConstraintMap = Collections.emptyMap();

		// This is a fixed set of property names that have associated constraints so we turn it into an unmodifiable set
		if (!s.isEmpty())
			constrainedProperties = Collections.unmodifiableSet(s);
		else
			constrainedProperties = Collections.emptySet();
	}

	/**
	 * Add constraints
	 * @param constraints
	 * @param thisClass
	 * @param methodName
	 * @param methodParameterTypes
	 */
	private void addConstraints(Set<Annotation> constraints, Class<?> thisClass, String methodName,
			Class<?>[] methodParameterTypes) {
		try {
			final Method method = thisClass.getDeclaredMethod(methodName, methodParameterTypes);

			// Add all constraints that are on the method
			addConstraints(constraints, method.getAnnotations());
		}
		catch (final NoSuchMethodException e) {
			// Ignored!
		}

		// If we have overridden the setter method then we may have annotations on the superclass method. So call recursively with our
		// superclass until we are at the top
		final Class<?> superClass = thisClass.getSuperclass();

		if (superClass != null)
			addConstraints(constraints, superClass, methodName, methodParameterTypes);
	}

	/**
	 * Add constraints
	 * @param constraints
	 * @param annotations
	 */
	private void addConstraints(Set<Annotation> constraints, Annotation[] annotations) {
		for (final Annotation a : annotations)
			if (isConstraint(a))
				constraints.add(a);
	}

}
