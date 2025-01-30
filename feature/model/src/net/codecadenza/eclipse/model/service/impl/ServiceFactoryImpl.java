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
package net.codecadenza.eclipse.model.service.impl;

import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.ScheduledInvocation;
import net.codecadenza.eclipse.model.service.ServiceFactory;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class ServiceFactoryImpl extends EFactoryImpl implements ServiceFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static ServiceFactory init() {
		try {
			final var theServiceFactory = (ServiceFactory) EPackage.Registry.INSTANCE.getEFactory(ServicePackage.eNS_URI);

			if (theServiceFactory != null)
				return theServiceFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new ServiceFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case ServicePackage.SCHEDULED_INVOCATION -> createScheduledInvocation();
			case ServicePackage.ASYNCHRONOUS_INVOCATION -> createAsynchronousInvocation();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceFactory#createScheduledInvocation()
	 * @generated
	 */
	@Override
	public ScheduledInvocation createScheduledInvocation() {
		return new ScheduledInvocationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceFactory#createAsynchronousInvocation()
	 * @generated
	 */
	@Override
	public AsynchronousInvocation createAsynchronousInvocation() {
		return new AsynchronousInvocationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceFactory#getServicePackage()
	 * @generated
	 */
	@Override
	public ServicePackage getServicePackage() {
		return (ServicePackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the service package
	 * @generated
	 */
	@Deprecated
	public static ServicePackage getPackage() {
		return ServicePackage.eINSTANCE;
	}

}
