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
package net.codecadenza.eclipse.tools.util.eclipse;

import java.util.HashSet;
import java.util.Set;

/**
 * <p>
 * Utility class for getting all necessary plug-ins of an Eclipse RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseRAPPlugins {
	/**
	 * Prevent instantiation
	 */
	private EclipseRAPPlugins() {

	}

	/**
	 * Get all plug-ins for running an Eclipse RAP application
	 * @return a list that contains all plug-ins
	 */
	public static final Set<PluginRecord> getPlugins() {
		final Set<PluginRecord> plugins = new HashSet<>();
		plugins.add(new PluginRecord("com.ibm.icu"));
		plugins.add(new PluginRecord("jakarta.annotation-api*2.1.1"));
		plugins.add(new PluginRecord("jakarta.inject.jakarta.inject-api"));
		plugins.add(new PluginRecord("jakarta.servlet-api"));
		plugins.add(new PluginRecord("org.apache.aries.spifly.dynamic.bundle"));
		plugins.add(new PluginRecord("org.apache.commons.collections"));
		plugins.add(new PluginRecord("org.apache.commons.commons-beanutils"));
		plugins.add(new PluginRecord("org.apache.commons.commons-fileupload2-core"));
		plugins.add(new PluginRecord("org.apache.commons.commons-fileupload2-jakarta-servlet6"));
		plugins.add(new PluginRecord("org.apache.commons.commons-io"));
		plugins.add(new PluginRecord("org.apache.commons.logging"));
		plugins.add(new PluginRecord("org.apache.felix.gogo.command"));
		plugins.add(new PluginRecord("org.apache.felix.gogo.runtime"));
		plugins.add(new PluginRecord("org.apache.felix.gogo.shell"));
		plugins.add(new PluginRecord("org.apache.felix.scr", "1:true"));
		plugins.add(new PluginRecord("org.eclipse.core.commands"));
		plugins.add(new PluginRecord("org.eclipse.core.contenttype"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding.beans"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding.observable"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding.property"));
		plugins.add(new PluginRecord("org.eclipse.core.databinding"));
		plugins.add(new PluginRecord("org.eclipse.core.expressions"));
		plugins.add(new PluginRecord("org.eclipse.core.jobs"));
		plugins.add(new PluginRecord("org.eclipse.core.runtime", "default:true"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.commands"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.contexts"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di.annotations"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di.extensions.supplier"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di.extensions"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.di"));
		plugins.add(new PluginRecord("org.eclipse.e4.core.services"));
		plugins.add(new PluginRecord("org.eclipse.e4.emf.xpath"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.bindings"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.di"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.model.workbench"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.services"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench.addons.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench.renderers.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench.swt"));
		plugins.add(new PluginRecord("org.eclipse.e4.ui.workbench"));
		plugins.add(new PluginRecord("org.eclipse.emf.common"));
		plugins.add(new PluginRecord("org.eclipse.emf.ecore.change"));
		plugins.add(new PluginRecord("org.eclipse.emf.ecore.xmi"));
		plugins.add(new PluginRecord("org.eclipse.emf.ecore"));
		plugins.add(new PluginRecord("org.eclipse.equinox.app"));
		plugins.add(new PluginRecord("org.eclipse.equinox.common", "2:true"));
		plugins.add(new PluginRecord("org.eclipse.equinox.console"));
		plugins.add(new PluginRecord("org.eclipse.equinox.event"));
		plugins.add(new PluginRecord("org.eclipse.equinox.preferences"));
		plugins.add(new PluginRecord("org.eclipse.equinox.registry"));
		plugins.add(new PluginRecord("org.eclipse.jetty.ee10.servlet"));
		plugins.add(new PluginRecord("org.eclipse.jetty.http"));
		plugins.add(new PluginRecord("org.eclipse.jetty.io"));
		plugins.add(new PluginRecord("org.eclipse.jetty.logging"));
		plugins.add(new PluginRecord("org.eclipse.jetty.security"));
		plugins.add(new PluginRecord("org.eclipse.jetty.server"));
		plugins.add(new PluginRecord("org.eclipse.jetty.session"));
		plugins.add(new PluginRecord("org.eclipse.jetty.util.ajax"));
		plugins.add(new PluginRecord("org.eclipse.jetty.util"));
		plugins.add(new PluginRecord("org.eclipse.osgi.util"));
		plugins.add(new PluginRecord("org.eclipse.osgi", "1:true"));
		plugins.add(new PluginRecord("org.eclipse.rap.e4"));
		plugins.add(new PluginRecord("org.eclipse.rap.filedialog"));
		plugins.add(new PluginRecord("org.eclipse.rap.fileupload"));
		plugins.add(new PluginRecord("org.eclipse.rap.http.jetty"));
		plugins.add(new PluginRecord("org.eclipse.rap.http.servlet"));
		plugins.add(new PluginRecord("org.eclipse.rap.jface.databinding"));
		plugins.add(new PluginRecord("org.eclipse.rap.jface"));
		plugins.add(new PluginRecord("org.eclipse.rap.rwt.osgi"));
		plugins.add(new PluginRecord("org.eclipse.rap.rwt"));
		plugins.add(new PluginRecord("org.objectweb.asm.commons"));
		plugins.add(new PluginRecord("org.objectweb.asm.tree.analysis"));
		plugins.add(new PluginRecord("org.objectweb.asm.tree"));
		plugins.add(new PluginRecord("org.objectweb.asm.util"));
		plugins.add(new PluginRecord("org.objectweb.asm"));
		plugins.add(new PluginRecord("org.osgi.service.cm"));
		plugins.add(new PluginRecord("org.osgi.service.component"));
		plugins.add(new PluginRecord("org.osgi.service.device"));
		plugins.add(new PluginRecord("org.osgi.service.event"));
		plugins.add(new PluginRecord("org.osgi.service.metatype"));
		plugins.add(new PluginRecord("org.osgi.service.prefs"));
		plugins.add(new PluginRecord("org.osgi.service.provisioning"));
		plugins.add(new PluginRecord("org.osgi.service.upnp"));
		plugins.add(new PluginRecord("org.osgi.service.useradmin"));
		plugins.add(new PluginRecord("org.osgi.service.wireadmin"));
		plugins.add(new PluginRecord("org.osgi.util.function"));
		plugins.add(new PluginRecord("org.osgi.util.measurement"));
		plugins.add(new PluginRecord("org.osgi.util.position"));
		plugins.add(new PluginRecord("org.osgi.util.promise"));
		plugins.add(new PluginRecord("org.osgi.util.xml"));
		plugins.add(new PluginRecord("slf4j.api"));

		return plugins;
	}

}
