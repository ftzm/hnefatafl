/**
 * Simple FIFO mutual-exclusion lock for async operations.
 * Queued callers are resolved in order of arrival.
 */
export class AsyncLock {
  private _lockQueue: Array<() => void> = [];
  private _isLocked = false;

  async acquire(): Promise<void> {
    return new Promise((resolve) => {
      if (!this._isLocked) {
        this._isLocked = true;
        resolve();
      } else {
        this._lockQueue.push(resolve);
      }
    });
  }

  release(): void {
    if (this._lockQueue.length > 0) {
      const [nextResolve] = this._lockQueue.splice(0, 1);
      nextResolve();
    } else {
      this._isLocked = false;
    }
  }

  async withLock<T>(fn: () => Promise<T>): Promise<T> {
    await this.acquire();
    try {
      return await fn();
    } finally {
      this.release();
    }
  }
}
