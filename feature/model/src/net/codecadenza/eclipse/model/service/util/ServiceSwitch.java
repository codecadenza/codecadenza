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
package net.codecadenza.eclipse.model.service.util;

import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.MethodInvocation;
import net.codecadenza.eclipse.model.service.ScheduledInvocation;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.service.ServicePackage
 * @generated
 */
public class ServiceSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ServicePackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public ServiceSwitch() {
		if (modelPackage == null)
			modelPackage = ServicePackage.eINSTANCE;
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
			case ServicePackage.SERVICE_BEAN: {
				final var serviceBean = (ServiceBean) theEObject;
				T result = caseServiceBean(serviceBean);

				if (result == null)
					result = caseJavaType(serviceBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ServicePackage.SERVICE_METHOD: {
				final var serviceMethod = (ServiceMethod) theEObject;
				T result = caseServiceMethod(serviceMethod);

				if (result == null)
					result = caseJavaMethod(serviceMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ServicePackage.METHOD_INVOCATION: {
				final var methodInvocation = (MethodInvocation) theEObject;
				T result = caseMethodInvocation(methodInvocation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ServicePackage.SCHEDULED_INVOCATION: {
				final var scheduledInvocation = (ScheduledInvocation) theEObject;
				T result = caseScheduledInvocation(scheduledInvocation);

				if (result == null)
					result = caseMethodInvocation(scheduledInvocation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ServicePackage.ASYNCHRONOUS_INVOCATION: {
				final var asynchronousInvocation = (AsynchronousInvocation) theEObject;
				T result = caseAsynchronousInvocation(asynchronousInvocation);

				if (result == null)
					result = caseMethodInvocation(asynchronousInvocation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Service Bean</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Service Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseServiceBean(ServiceBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Service Method</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Service Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseServiceMethod(ServiceMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Method Invocation</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Method Invocation</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseMethodInvocation(MethodInvocation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Scheduled Invocation</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Scheduled Invocation</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseScheduledInvocation(ScheduledInvocation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Asynchronous Invocation</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Asynchronous Invocation</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAsynchronousInvocation(AsynchronousInvocation object) {
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
