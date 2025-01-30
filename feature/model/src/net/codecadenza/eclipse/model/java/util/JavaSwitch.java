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
package net.codecadenza.eclipse.model.java.util;

import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.java.JavaPackage
 * @generated
 */
public class JavaSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static JavaPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public JavaSwitch() {
		if (modelPackage == null)
			modelPackage = JavaPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#isSwitchFor(org.eclipse.emf.ecore.EPackage)
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#doSwitch(int, org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case JavaPackage.ENUM_LITERAL: {
				final var enumLiteral = (EnumLiteral) theEObject;
				T result = caseEnumLiteral(enumLiteral);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case JavaPackage.JAVA_ENUM: {
				final var javaEnum = (JavaEnum) theEObject;
				T result = caseJavaEnum(javaEnum);

				if (result == null)
					result = caseJavaType(javaEnum);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case JavaPackage.JAVA_METHOD: {
				final var javaMethod = (JavaMethod) theEObject;
				T result = caseJavaMethod(javaMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case JavaPackage.JAVA_TYPE: {
				final var javaType = (JavaType) theEObject;
				T result = caseJavaType(javaType);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case JavaPackage.METHOD_PARAMETER: {
				final var methodParameter = (MethodParameter) theEObject;
				T result = caseMethodParameter(methodParameter);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case JavaPackage.NAMESPACE: {
				final var namespace = (Namespace) theEObject;
				T result = caseNamespace(namespace);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Enum Literal</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Enum Literal</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseEnumLiteral(EnumLiteral object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Enum</em>'. This implementation returns null; returning a
	 * non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Enum</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaEnum(JavaEnum object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Java Method</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Java Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaMethod(JavaMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Java Type</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Java Type</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaType(JavaType object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Method Parameter</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Method Parameter</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseMethodParameter(MethodParameter object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Namespace</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Namespace</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseNamespace(Namespace object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#defaultCase(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

}
