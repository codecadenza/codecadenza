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
package net.codecadenza.eclipse.diagram.domain.expressions;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.Map;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * <p>
 * Abstract expression
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class CodeCadenzaAbstractExpression {
	private IStatus status = Status.OK_STATUS;

	/**
	 * @param severity
	 * @param message
	 * @param throwable
	 */
	protected void setStatus(int severity, String message, Throwable throwable) {
		final String pluginID = CodeCadenzaDiagramEditorPlugin.ID;
		this.status = new Status(severity, pluginID, -1, (message != null) ? message : "", throwable);

		if (!this.status.isOK())
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Expression problem:" + message + "body:" + body(), throwable);
	}

	/**
	 * @return the status
	 */
	public IStatus getStatus() {
		return status;
	}

	private final String myBody;

	/**
	 * @return the body
	 */
	public String body() {
		return myBody;
	}

	private final EClassifier myContext;

	/**
	 * @return the context
	 */
	public EClassifier context() {
		return myContext;
	}

	/**
	 * @param body
	 * @param context
	 */
	protected CodeCadenzaAbstractExpression(String body, EClassifier context) {
		myBody = body;
		myContext = context;
	}

	protected abstract Object doEvaluate(Object context, Map<?, ?> env);

	/**
	 * @param context
	 * @return an object
	 */
	public Object evaluate(Object context) {
		return evaluate(context, Collections.emptyMap());
	}

	/**
	 * @param context
	 * @param env
	 * @return an object
	 */
	public Object evaluate(Object context, Map<?, ?> env) {
		if (context().isInstance(context)) {
			try {
				return doEvaluate(context, env);
			}
			catch (final Exception e) {
				CodeCadenzaDiagramEditorPlugin.getInstance().logError("Expression evaluation failure: " + body(), e);
			}
		}

		return null;
	}

	/**
	 * @param value
	 * @param targetType
	 * @return an object. Note that it may return a number value which is not directly compatible with the feature type (e.g. Double
	 *         when Integer is expected)!
	 */
	public static Object performCast(Object value, EDataType targetType) {
		if (targetType instanceof EEnum && value instanceof final EEnumLiteral literal)
			return (literal.getInstance() != null) ? literal.getInstance() : literal;

		if (!(value instanceof final Number num) || targetType == null || targetType.getInstanceClass() == null)
			return value;

		final Class<?> targetClass = targetType.getInstanceClass();
		final Class<?> valClass = value.getClass();
		Class<?> targetWrapperClass = targetClass;

		if (targetClass.isPrimitive())
			targetWrapperClass = EcoreUtil.wrapperClassFor(targetClass);

		if (valClass.equals(targetWrapperClass))
			return value;

		if (Number.class.isAssignableFrom(targetWrapperClass)) {
			if (targetWrapperClass.equals(Byte.class))
				return Byte.valueOf(num.byteValue());

			if (targetWrapperClass.equals(Integer.class))
				return Integer.valueOf(num.intValue());

			if (targetWrapperClass.equals(Short.class))
				return Short.valueOf(num.shortValue());

			if (targetWrapperClass.equals(Long.class))
				return Long.valueOf(num.longValue());

			if (targetWrapperClass.equals(BigInteger.class))
				return BigInteger.valueOf(num.longValue());

			if (targetWrapperClass.equals(Float.class))
				return Float.valueOf(num.floatValue());

			if (targetWrapperClass.equals(Double.class))
				return Double.valueOf(num.doubleValue());

			if (targetWrapperClass.equals(BigDecimal.class))
				return BigDecimal.valueOf(num.doubleValue());
		}

		return value;
	}

}
