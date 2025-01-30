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
package net.codecadenza.eclipse.generator.integration.method;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.jms.BasicJMSMethodGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.kafka.BasicKafkaMethodGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.rest.RESTMethodGeneratorFactory;
import net.codecadenza.eclipse.generator.integration.method.imp.rmi.RMIMethodGeneratorFactory;
import net.codecadenza.eclipse.generator.integration.method.imp.soap.SOAPMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;

/**
 * <p>
 * Factory for integration method generators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationMethodGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private IntegrationMethodGeneratorFactory() {

	}

	/**
	 * @param integrationMethod
	 * @param parentGenerator
	 * @return a generator for the given integration technology
	 * @throws IllegalStateException if an implementation for the given integration technology is not available
	 */
	public static AbstractIntegrationMethodGenerator getMethodGenerator(AbstractIntegrationMethod integrationMethod,
			AbstractJavaSourceGenerator parentGenerator) {
		final AbstractIntegrationBean integrationBean = integrationMethod.getIntegrationBean();

		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.SOAP)
			return SOAPMethodGeneratorFactory.getMethodGenerator((SOAPIntegrationMethod) integrationMethod, parentGenerator);
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.REST)
			return RESTMethodGeneratorFactory.getMethodGenerator((RESTIntegrationMethod) integrationMethod, parentGenerator);
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.RMI)
			return RMIMethodGeneratorFactory.getMethodGenerator((RMIIntegrationMethod) integrationMethod, parentGenerator);
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			return new BasicKafkaMethodGenerator((KafkaIntegrationMethod) integrationMethod, parentGenerator);
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.JMS)
			return new BasicJMSMethodGenerator((JMSIntegrationMethod) integrationMethod, parentGenerator);

		final var msg = "An integration method generator for the technology '" + integrationBean.getIntegrationTechnology()
				+ "' is not available!";

		throw new IllegalStateException(msg);
	}

}
