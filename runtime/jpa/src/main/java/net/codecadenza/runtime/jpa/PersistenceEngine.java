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
package net.codecadenza.runtime.jpa;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.Persistence;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Helper class that encapsulates the creation of an entity manager and an entity manager factory. Note that this implementation
 * expects that the persistence unit name is '_default'!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PersistenceEngine {
	public static final String DEFAULT_PERSISTENCE_UNIT_NAME = "_default";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static EntityManagerFactory emf;
	private static boolean started;
	private static final int INIT_POOL_SIZE = 10;
	private static HashMap<EntityManager, Boolean> entityManagerPool = new HashMap<>();

	/**
	 * Constructor
	 */
	private PersistenceEngine() {

	}

	/**
	 * Startup the persistence engine
	 */
	public static synchronized void startup() {
		logger.debug("Startup persistence engine");

		emf = Persistence.createEntityManagerFactory(DEFAULT_PERSISTENCE_UNIT_NAME);

		for (int i = 0; i < INIT_POOL_SIZE; i++)
			entityManagerPool.put(emf.createEntityManager(), false);

		started = true;
	}

	/**
	 * @return an entity manager instance from an internal pool
	 */
	public static synchronized EntityManager getEntityManager() {
		if (!started)
			startup();

		// Search for a "free" instance in the pool!
		for (final Map.Entry<EntityManager, Boolean> entry : entityManagerPool.entrySet())
			if (Boolean.FALSE.equals(entry.getValue())) {
				entityManagerPool.put(entry.getKey(), true);

				logger.trace("Return entity manager from pool");

				return entry.getKey();
			}

		// A new instance must be created and it must be added to the pool!
		final EntityManager em = emf.createEntityManager();

		entityManagerPool.put(em, true);

		logger.trace("Return new entity manager");

		return em;
	}

	/**
	 * Release the entity manager
	 * @param em
	 */
	public static synchronized void releaseEntityManager(EntityManager em) {
		if (!entityManagerPool.containsKey(em))
			return;

		logger.trace("Release entity manager");

		// Detach all entities in order to avoid working with stale objects when reusing the entity manager in subsequent operations!
		em.clear();

		entityManagerPool.put(em, false);
	}

	/**
	 * Shutdown the persistence engine
	 */
	public static synchronized void shutdown() {
		if (started && emf != null && emf.isOpen()) {
			logger.debug("Shutdown persistence engine");

			entityManagerPool.keySet().stream().filter(EntityManager::isOpen).forEach(EntityManager::close);

			emf.close();
		}
	}

}
